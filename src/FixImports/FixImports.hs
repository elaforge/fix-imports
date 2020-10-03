{- | Automatically fix the import list in a haskell module.

    The process is as follows:

    - Parse the entire file and extract the Qualification of qualified names
    like @A.b@, which is simple @A@.

    - Combine this with the modules imported to decide which imports can be
    removed and which ones must be added.

    - For added imports, guess the complete import path implied by the
    Qualification.  This requires some heuristics:

        - Check local modules first.  Start in the current module's directory
        and then try from the current directory, descending recursively.

        - If no local modules are found, check the package database.  There is
        a system of package priorities so that @List@ will yield @Data.List@
        from @base@ rather than @List@ from @haskell98@.  After that, shorter
        matches are prioritized so @System.Process@ is chosen over
        @System.Posix.Process@.

        - If the module is not found at all, an error is printed on stderr and
        the unchanged file on stdout.

        - Of course the heuristics may get the wrong module, but existing
        imports are left alone so you can edit them by hand.

    - Then imports are sorted, grouped, and a new module is written to stdout
    with the new import block replacing the old one.

        - The default import formatting separates package imports from local
        imports, and groups them by their toplevel module name (before the
        first dot).  Small groups are combined.  They go in alphabetical order
        by default, but a per-project order may be defined.
-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
module FixImports.FixImports where
import Prelude hiding (mod)
import Control.Applicative ((<$>))
import qualified Control.Monad.State.Strict as State
import qualified Control.DeepSeq as DeepSeq
import Control.Monad.Trans (lift)
import Data.Bifunctor (first)
import qualified Data.Char as Char
import qualified Data.Either as Either
import qualified Data.Generics.Uniplate.Data as Uniplate
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text
import Data.Text (Text)
import qualified Data.Time.Clock as Clock
import qualified Data.Tuple as Tuple

import qualified Language.Haskell.Exts as Haskell
import qualified Language.Haskell.Exts.Extension as Extension
import qualified Language.Preprocessor.Cpphs as Cpphs

import qualified Numeric
import qualified System.Directory as Directory
import qualified System.FilePath as FilePath
import System.FilePath ((</>))

import qualified FixImports.Config as Config
import qualified FixImports.Index as Index
import qualified FixImports.Types as Types
import qualified FixImports.Util as Util

import Control.Monad


-- | Look only this deep for local modules.
searchDepth :: Int
searchDepth = 12

data Result = Result {
    resultRange :: (Row, Row)
    , resultImports :: String
    , resultAdded :: Set.Set Types.ModuleName
    , resultRemoved :: Set.Set Types.ModuleName
    , resultMetrics :: [Metric]
    } deriving (Show)

-- | Line number in the input file.
type Row = Int

type Metric = (Clock.UTCTime, Text)

addMetrics :: [Metric] -> Result -> Result
addMetrics ms result = result { resultMetrics = ms ++ resultMetrics result }

fixModule :: Config.Config -> FilePath -> String
    -> IO (Either String Result, [Text])
fixModule config modulePath source = do
    mStart <- metric () "start"
    processedSource <- cppModule modulePath source
    mCpp <- metric () "cpp"
    case parse (Config._language config) modulePath processedSource of
        Haskell.ParseFailed srcloc err ->
            return (Left $ Haskell.prettyPrint srcloc ++ ": " ++ err, [])
        Haskell.ParseOk (mod, cmts) -> do
            mParse <- metric (mod `seq` (), cmts) "parse"
            index <- Index.load
            mLoad <- metric () "load-index"
            fmap (fmap List.reverse) $ flip State.runStateT [] $
                fmap (addMetrics [mStart, mCpp, mParse, mLoad]) <$>
                fixImports ioFilesystem config index modulePath mod cmts

parse :: [Extension.Extension] -> FilePath -> String
    -> Haskell.ParseResult
        (Haskell.Module Haskell.SrcSpanInfo, [Haskell.Comment])
parse extensions modulePath =
    Haskell.parseFileContentsWithComments $ Haskell.defaultParseMode
        { Haskell.parseFilename = modulePath
        , Haskell.extensions = extensions ++ defaultExtensions
        -- The meaning of Nothing is undocumented, but I think it means
        -- to not check for fixity ambiguity at all, which is what I want.
        -- , Haskell.fixities = Nothing
        , Haskell.ignoreFunctionArity = False
        }
    where
    defaultExtensions = map Extension.EnableExtension $
        Extension.toExtensionList Extension.Haskell2010 []
        -- GHC has this extension enabled by default, and it's easy
        -- to wind up with code that relies on it:
        -- http://www.haskell.org/ghc/docs/7.6.3/html/users_guide/bugs-and-infelicities.html#infelicities-syntax
        ++ [Extension.NondecreasingIndentation]

-- | The parse function takes a CPP extension, but doesn't actually pay any
-- attention to it, so I have to run CPP myself.  The imports are fixed
-- post-CPP so if you put CPP in the imports block it will be stripped out.
-- It seems hard to fix imports inside CPP.
cppModule :: FilePath -> String -> IO String
cppModule filename = Cpphs.runCpphs options filename
    where
    options = Cpphs.defaultCpphsOptions { Cpphs.boolopts = boolOpts }
    boolOpts = Cpphs.defaultBoolOptions
        { Cpphs.macros = True
        , Cpphs.locations = False
        , Cpphs.hashline = False
        , Cpphs.pragma = False
        , Cpphs.stripEol = True
        , Cpphs.stripC89 = True
        , Cpphs.lang = True -- lex input as haskell code
        , Cpphs.ansi = True
        , Cpphs.layout = True
        , Cpphs.literate = False -- untested with literate code
        , Cpphs.warnings = False
        }

-- | Capture all the IO operations needed by fixImports, so I can test without
-- IO.  I could have used Free, but the operations are few, so it seemed
-- simpler to factor them out.
data Filesystem m = Filesystem {
    _listDir :: FilePath -> m ([FilePath], [FilePath]) -- ^ (dirs, files)
    , _doesFileExist :: FilePath -> m Bool
    , _metric :: forall a. DeepSeq.NFData a => a -> Text -> m Metric
    }

ioFilesystem :: Filesystem IO
ioFilesystem = Filesystem
    { _listDir = \ dir -> do
        fns <- Maybe.fromMaybe [] <$> Util.catchENOENT (Util.listDir dir)
        Util.partitionM Directory.doesDirectoryExist fns
    , _doesFileExist = Directory.doesFileExist
    , _metric = metric
    }

type LogT m a = State.StateT [Text] m a

debug :: Monad m => Config.Config -> Text -> LogT m ()
debug config msg = when (Config._debug config) $ State.modify' (msg:)
    -- The check is unnecessary since I check debug before printing them, but
    -- it'll save a thunk at least.

-- | Take a parsed module along with its unparsed text.  Generate a new import
-- block with proper spacing, formatting, and comments.  Then snip out the
-- import block on the import file, and replace it.
fixImports :: Monad m => Filesystem m -> Config.Config -> Index.Index
    -> FilePath -> Types.Module -> [Haskell.Comment]
    -> LogT m (Either String Result)
fixImports fs config index modulePath mod cmts = do
    let (newImports, unusedImports, imports, range) = importInfo mod cmts
    mProcess <- lift $ _metric fs
        (length newImports, length unusedImports, length imports, range)
        "process"
    mbNew <- mapM (findNewImport fs config modulePath index)
        (Set.toList newImports)
    mNewImports <- lift $ _metric fs mbNew "find-new-imports"
    (imports, newUnqualImports, unusedUnqual) <- return $
        fixUnqualified config mod imports
    newUnqualImports <- lift $ mapM (locateImport fs) newUnqualImports
    mUnqual <- lift $ _metric fs newUnqualImports "unqualified-imports"
    mbExisting <- mapM (findImport fs index (Config._includes config)) imports
    mExistingImports <- lift $ _metric fs mbExisting "find-existing-imports"
    let existing = map (Types.importDeclModule . fst) imports
    let (notFound, importLines) = Either.partitionEithers $
            zipWith mkError
                (map toModule (Set.toList newImports) ++ existing)
                (mbNew ++ mbExisting)
        mkError _ (Just imp) = Right imp
        mkError mod Nothing = Left mod
    let formattedImports =
            Config.formatGroups (Config._format config) (Config._order config)
                (importLines ++ newUnqualImports)
    return $ case notFound of
        _ : _ -> Left $ "not found: "
            ++ Util.join ", " (map Types.moduleName notFound)
        [] -> Right $ Result
            { resultRange = range
            , resultImports = formattedImports
            , resultAdded = Set.fromList $
                map (Types.importDeclModule . Types.importDecl) $
                Maybe.catMaybes mbNew ++ newUnqualImports
            , resultRemoved = unusedImports <> Set.fromList unusedUnqual
            , resultMetrics =
                [mProcess, mNewImports, mExistingImports, mUnqual]
            }
    where
    toModule (Types.Qualification name) = Types.ModuleName name

locateImport :: Monad m => Filesystem m -> ImportLine -> m Types.ImportLine
locateImport fs (decl, cmts) = do
    isLocal <- _doesFileExist fs $
        Types.moduleToPath $ Types.importDeclModule decl
    return $ Types.ImportLine
        { importDecl = decl
        , importComments = cmts
        , importSource = if isLocal then Types.Local else Types.Package
        }

-- | This is like 'Types.ImportLine', except without Local/Package info.
type ImportLine = (Types.ImportDecl, [Types.Comment])


-- | Add unqualified imports.
--
-- - Get unqualifieds.
-- - If _unqualified non-empty, filter them to the ones in _unqualified.
-- - Add or modify import lines for them.
-- - Remove imports that don't appear in unqualifiedImports
fixUnqualified :: Config.Config -> Types.Module -> [ImportLine]
    -> ([ImportLine], [ImportLine], [Types.ModuleName])
    -- ^ (modified, new, removed)
fixUnqualified config mod imports =
    -- Add new references.
    -- Then filter out managed symbols which are no longer present.
    -- Then delete empty imports which are now empty.
    removeEmptyImports . first (map stripReferences)
        . foldr addReferences (imports, []) . Map.toList $ references
    where
    removeEmptyImports (modified, new) =
        (kept, new, map (Types.importDeclModule . fst) removed)
        where
        (kept, removed) = List.partition (not . emptyImport . fst) modified
    references = unqualifiedImports config mod
    emptyImport decl = and
        [ Map.member moduleName moduleToNames
        -- Can delete if it has an import list, but it's empty.
        , hasEmptyImportList decl
        ]
        where moduleName = Types.importDeclModule decl

    stripReferences (decl, cmts) =
        ( modifyImportSpecs (filter (keep (Types.importDeclModule decl))) decl
        , cmts
        )
        where
        -- Keep if it's not managed, or it is managed and referenced.
        keep moduleName name = not (isManaged moduleName name)
            || maybe False (name `elem`) (Map.lookup moduleName references)
    addReferences (moduleName, names) (existing, new) =
        case Util.modifyAt (matches moduleName . fst) add existing of
            Nothing -> (existing, (newImport, []) : new)
            Just modified -> (modified, new)
        where
        add = first $ modifyImportSpecs (names++)
        newImport = modifyImportSpecs (names++) $
            mkImportDecl moduleName Nothing True
    matches name decl = isUnqualifiedImport decl
        && Types.importDeclModule decl == name
    isUnqualifiedImport decl = not (Haskell.importQualified decl)
        && maybe True (not . importSpecsHiding) (Haskell.importSpecs decl)

    isManaged moduleName name = maybe False (name `elem`) $
        Map.lookup moduleName moduleToNames
    moduleToNames :: Map.Map Types.ModuleName [Haskell.Name ()]
    moduleToNames = Util.multimap . map Tuple.swap . Map.toList
        . Config._unqualified $ config

-- | This field seems to be undocumented.
importSpecsHiding :: Haskell.ImportSpecList a -> Bool
importSpecsHiding (Haskell.ImportSpecList _ hiding _) = hiding

hasEmptyImportList :: Haskell.ImportDecl a -> Bool
hasEmptyImportList decl = case Haskell.importSpecs decl of
    Just (Haskell.ImportSpecList _ _ specs) -> null specs
    Nothing -> False

-- | If this import has a import list, modify its contents.
--
-- I strip SrcSpanInfo from the Names because otherwise sorting and unique'ing
-- is too finicky.
modifyImportSpecs :: ([Haskell.Name ()] -> [Haskell.Name ()])
    -> Types.ImportDecl -> Types.ImportDecl
modifyImportSpecs modify decl = case Haskell.importSpecs decl of
    Just specs | not (importSpecsHiding specs) -> decl
        { Haskell.importSpecs = Just $ modifySpecs specs
        }
    _ -> decl
    where
    -- hiding should be False due to the match above.
    modifySpecs (Haskell.ImportSpecList span _hiding specs) =
        Haskell.ImportSpecList span False $ Set.toList $ Set.fromList $
            map makeVar (doModify vars) ++ others
        where
        (vars, others) = Util.partitionOn varOf specs
        varOf (Haskell.IVar _ name) = Just name
        varOf (Haskell.IAbs _ (Haskell.NoNamespace _) name) = Just name
        varOf _ = Nothing
    doModify = map addSpan . modify . map stripSpan
    -- IAbs is constructors and classes, but I can disambiguate just by seeing
    -- if it's capitalized.
    makeVar name
        | isCaps name = Haskell.IAbs noSpan (Haskell.NoNamespace noSpan) name
        | otherwise = Haskell.IVar noSpan name
    isCaps (Haskell.Ident _ name) = all Char.isUpper (take 1 name)
    isCaps _ = False

stripSpan :: Functor f => f Haskell.SrcSpanInfo -> f ()
stripSpan = fmap (const ())

addSpan :: Functor f => f () -> f Haskell.SrcSpanInfo
addSpan = fmap (const noSpan)

unqualifiedImports :: Config.Config -> Types.Module
    -> Map.Map Types.ModuleName [Haskell.Name ()]
unqualifiedImports config mod
    | unqual == mempty = mempty
    | otherwise = Util.multimap $
        Maybe.mapMaybe (\k -> (, k) <$> Map.lookup k unqual) $
        map stripSpan $ moduleUnqualifieds mod
    where unqual = Config._unqualified config

-- | Clip out the range from the given text and replace it with the given
-- lines.
substituteImports :: String -> (Int, Int) -> String -> String
substituteImports imports (start, end) source =
    unlines pre ++ imports ++ unlines post
    where
    (pre, within) = splitAt start (lines source)
    (_, post) = splitAt (end-start) within

-- * find new imports

-- | Make a new ImportLine from a ModuleName.
findNewImport :: Monad m => Filesystem m -> Config.Config -> FilePath
    -> Index.Index -> Types.Qualification
    -> LogT m (Maybe Types.ImportLine)
    -- ^ Nothing if the module wasn't found
findNewImport fs config modulePath index qual =
    fmap make <$> findModule fs config index modulePath qual
    where
    make (mod, source) = Types.ImportLine
        { importDecl = mkImportDecl mod (Just qual) False
        , importComments = []
        , importSource = source
        }

mkImportDecl :: Types.ModuleName -> Maybe Types.Qualification -> Bool
    -> Types.ImportDecl
mkImportDecl (Types.ModuleName name) qualification withImportList =
    Haskell.ImportDecl
        { Haskell.importAnn = noSpan
        , Haskell.importModule = Haskell.ModuleName noSpan name
        , Haskell.importQualified = Maybe.isJust qualification
        , Haskell.importSrc = False
        , Haskell.importSafe = False
        , Haskell.importPkg = Nothing
        , Haskell.importAs = Haskell.ModuleName noSpan <$> importAs
        , Haskell.importSpecs = if withImportList
            then Just $ Haskell.ImportSpecList noSpan False []
            else Nothing
        }
    where
    importAs
        -- | Just (Types.Qualification importAs) <- mbImportAs = Just importAs
        | Just (Types.Qualification q) <- qualification, q /= name = Just q
        | otherwise = Nothing

noSpan :: Haskell.SrcSpanInfo
noSpan = Haskell.noInfoSpan (Haskell.SrcSpan "" 0 0 0 0)

-- | Find the qualification and its ModuleName and whether it was a Types.Local
-- or Types.Package module.  Nothing if it wasn't found at all.
findModule :: Monad m => Filesystem m -> Config.Config -> Index.Index
    -> FilePath -- ^ Path to the module being fixed.
    -> Types.Qualification -> LogT m (Maybe (Types.ModuleName, Types.Source))
findModule fs config index modulePath qual = do
    local <- map fnameToModule <$> ((++)
        <$> findLocalModules fs (Config._includes config) qual
        <*> maybe (pure []) (findLocalModules fs (Config._includes config))
            qualifyAs)
    let package = map (first Just) $
            findPackageModules qual ++ maybe [] findPackageModules qualifyAs
    debug config $ "findModule " <> showt qual <> " from "
        <> showt modulePath <> ": local " <> showt local
        <> "\npackage: " <> showt package
    let prio = Config._modulePriority config
    return $ case Config.pickModule prio modulePath (local++package) of
        Just (package, mod) -> Just
            (mod, if package == Nothing then Types.Local else Types.Package)
        Nothing -> Nothing
    where
    findPackageModules q = Map.findWithDefault [] q index
    fnameToModule fn = (Nothing, Types.pathToModule fn)
    qualifyAs = Map.lookup qual (Config._qualifyAs config)

-- If it's in Config._qualifyAs, then I also search for exactly that module
-- name.

-- | Given A.B, look for A/B.hs, */A/B.hs, */*/A/B.hs, etc. in each of the
-- include paths.
findLocalModules :: Monad m => Filesystem m -> [FilePath]
    -> Types.Qualification -> LogT m [FilePath]
findLocalModules fs includes (Types.Qualification name) =
    fmap concat . forM includes $ \dir -> map (stripDir dir) <$>
        findFiles fs searchDepth (Types.moduleToPath (Types.ModuleName name))
            dir

stripDir :: FilePath -> FilePath -> FilePath
stripDir dir path
    | dir == "." = path
    | otherwise = dropWhile (=='/') $ drop (length dir) path

findFiles :: Monad m => Filesystem m
    -> Int -- ^ Descend into subdirectories this many times.
    -> FilePath -- ^ Find files with this suffix.  Can contain slashes.
    -> FilePath -- ^ Start from this directory.  Return [] if it doesn't exist.
    -> LogT m [FilePath]
findFiles fs depth file dir = do
    (subdirs, fns) <- lift $ _listDir fs dir
    subfns <- if depth > 0
        then concat <$> mapM (findFiles fs (depth-1) file)
            (filter isModuleDir subdirs)
        else return []
    return $ filter sameSuffix fns ++ subfns
    where
    isModuleDir = all Char.isUpper . take 1 . FilePath.takeFileName
    sameSuffix fn = fn == file || ('/' : file) `List.isSuffixOf` fn


-- * figure out existing imports

-- | Make an existing import into an ImportLine by finding out if it's a local
-- module or a package module.
findImport :: Monad m => Filesystem m -> Index.Index -> [FilePath] -> ImportLine
    -> LogT m (Maybe Types.ImportLine)
findImport fs index includes (imp, cmts) = do
    found <- findModuleName fs index includes (Types.importDeclModule imp)
    return $ case found of
        Nothing -> Nothing
        Just source -> Just $ Types.ImportLine
            { importDecl = imp
            , importComments = cmts
            , importSource = source
            }

-- | True if it was found in a local directory, False if it was found in the
-- ghc package db, and Nothing if it wasn't found at all.
findModuleName :: Monad m => Filesystem m -> Index.Index -> [FilePath]
    -> Types.ModuleName -> LogT m (Maybe Types.Source)
findModuleName fs index includes mod = do
    isLocal <- lift $ isLocalModule fs mod ("" : includes)
    return $
        if isLocal then Just Types.Local
        else if isPackageModule index mod then Just Types.Package
        else Nothing

isLocalModule :: Monad m => Filesystem m -> Types.ModuleName -> [FilePath]
    -> m Bool
isLocalModule fs mod =
    Util.anyM (_doesFileExist fs . (</> Types.moduleToPath mod))

isPackageModule :: Index.Index -> Types.ModuleName -> Bool
isPackageModule index (Types.ModuleName name) =
    Map.member (Types.Qualification name) index

-- * util

importInfo :: Types.Module -> [Haskell.Comment]
    ->  ( Set.Set Types.Qualification
        , Set.Set Types.ModuleName
        , [ImportLine]
        , (Int, Int)
        )
    -- ^ (missingImports, unusedImports, unchangedImports, rangeOfImportBlock)
importInfo mod cmts = (missing, unused, declCmts, range)
    where
    unused = Set.difference (Set.fromList modules)
        (Set.fromList (map (Types.importDeclModule . fst) declCmts))
    missing = Set.difference used imported
    -- If the Prelude isn't explicitly imported, it's implicitly imported, so
    -- if I see Prelude.x it doesn't mean to add an import.
    imported = Set.fromList $ prelude : qualifiedImports

    -- Get from the qualified import name back to the actual module name so
    -- I can return that.
    modules = map Types.importDeclModule imports
    qualifiedImports = map Types.importDeclQualification imports
    used = Set.fromList (moduleQNames mod)
    imports = moduleImportDecls mod
    declCmts =
        [ imp
        | imp@(decl, _) <- associateComments imports
            (filterImportCmts range cmts)
        , keepImport decl
        ]
    -- Keep unqualified imports, but only keep qualified ones if they are used.
    -- Prelude is considered always used if it appears, because removing it
    -- changes import behavour.
    keepImport decl =
        Set.member (Types.importDeclQualification decl)
            (Set.insert prelude used)
        || not (Haskell.importQualified decl)
    prelude = Types.Qualification "Prelude"
    range = importRange mod

filterImportCmts :: (Int, Int) -> [Haskell.Comment] -> [Haskell.Comment]
filterImportCmts (start, end) = filter inRange
    where
    inRange (Haskell.Comment _ src _) = start <= s && s < end
        where s = Haskell.srcSpanStartLine src - 1 -- spans are 1-based


-- | Pair ImportDecls up with the comments that apply to them.  Comments
-- below the last import are dropped, but there shouldn't be any of those
-- because they should have been omitted from the comment block.
--
-- Spaces between comments above an import will be lost, and multiple comments
-- to the right of an import (e.g. commenting a complicated import list) will
-- probably be messed up.  TODO Fix it if it becomes a problem.
associateComments :: [Types.ImportDecl] -> [Haskell.Comment] -> [ImportLine]
associateComments imports cmts = snd $ List.mapAccumL associate cmts imports
    where
    associate cmts imp = (after, (imp, associated))
        where
        associated = map (Types.Comment Types.CmtAbove . cmtText) above
            ++ map (Types.Comment Types.CmtRight . cmtText) right
        -- cmts that end before the import beginning are above it
        (above, rest) = List.span ((< start impSpan) . end . cmtSpan) cmts
        -- remaining cmts that start before or at the import's end are right
        -- of it
        (right, after) = List.span ((<= end impSpan) . start . cmtSpan) rest
        impSpan = Haskell.srcInfoSpan (Haskell.importAnn imp)
    cmtSpan (Haskell.Comment _ span _) = span
    cmtText (Haskell.Comment True _ s) = "{-" ++ s ++ "-}"
    cmtText (Haskell.Comment False _ s) = "--" ++ s
    start = Haskell.srcSpanStartLine
    end = Haskell.srcSpanEndLine

moduleImportDecls :: Types.Module -> [Types.ImportDecl]
moduleImportDecls (Haskell.Module _ _ _ imports _) = imports
moduleImportDecls _ = []

-- | Return half-open line range of import block, starting from (0 based) line
-- of first import to the line after the last one.
importRange :: Types.Module -> (Int, Int)
importRange mod = case mod of
        -- Haskell.Module _ mb_head _ imports _ ->
        --     let start = headEnd mb_head
        --     in (start, max start (importsEnd imports))
        Haskell.Module _ Nothing _ imports _ ->
            (importsStart imports, importsEnd imports)
        Haskell.Module _ (Just modHead) _ imports _ ->
            let start = headEnd modHead
            in (start, max start (importsEnd imports))
        _ -> (0, 0)
    where
    -- The parser counts lies from 1, but I return a half-open range from 0.
    -- So I don't need to +1 the last line of the head, and since the range
    -- is half-open I don't need to -1 the last line of the imports.
    headEnd (Haskell.ModuleHead src _ _ _) = endOf src
    importsStart [] = 0
    importsStart (importDecl : _) =
        startOf (Haskell.importAnn importDecl) - 1
    importsEnd [] = 0
    importsEnd imports = (endOf . Haskell.importAnn . last) imports

    startOf = Haskell.srcSpanStartLine . Haskell.srcInfoSpan
    endOf = Haskell.srcSpanEndLine . Haskell.srcInfoSpan

-- | Uniplate is rad.
moduleQNames :: Types.Module -> [Types.Qualification]
moduleQNames mod =
    [ Types.moduleToQualification m
    | Haskell.Qual _ m _ <- Uniplate.universeBi mod
    ]

moduleUnqualifieds :: Types.Module -> [Types.Name]
moduleUnqualifieds mod =
    [name | Haskell.UnQual _ name <- Uniplate.universeBi mod]

-- * metrics

metric :: DeepSeq.NFData a => a -> Text -> IO Metric
metric val name = do
    force val
    flip (,) name <$> Clock.getCurrentTime

showMetrics :: [Metric] -> Text
showMetrics = Text.unlines . format . map diff . Util.zipPrev . Util.sortOn fst
    where
    format metricDurs =
        map (format1 total) (metricDurs ++ [("total", total)])
        where total = sum (map snd metricDurs)
    format1 total (metric, dur) = Text.unwords
        [ justifyR 8 (showDuration dur)
        , justifyR 3 (percent (realToFrac dur / realToFrac total))
        , "-", metric
        ]
    diff ((prev, _), (cur, metric)) =
        (metric, cur `Clock.diffUTCTime` prev)

force :: DeepSeq.NFData a => a -> IO ()
force x = DeepSeq.rnf x `seq` return ()

percent :: Double -> Text
percent = (<>"%") . showt . isInt . round . (*100)
    where
    isInt :: Int -> Int
    isInt = id

showDuration :: Clock.NominalDiffTime -> Text
showDuration =
    Text.pack . ($"s") . Numeric.showFFloat (Just 2) . isDouble . realToFrac
    where
    isDouble :: Double -> Double
    isDouble = id

justifyR :: Int -> Text -> Text
justifyR width = Text.justifyRight width ' '

showt :: Show a => a -> Text
showt = Text.pack . show
