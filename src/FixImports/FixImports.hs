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
import qualified Control.Monad.State.Strict as State
import qualified Control.DeepSeq as DeepSeq
import           Control.Monad.Trans (lift)
import           Data.Bifunctor (first)
import qualified Data.Char as Char
import qualified Data.Either as Either
import qualified Data.List as List
import qualified Data.Map as Map
import           Data.Map (Map)
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import           Data.Set (Set)
import qualified Data.Text as Text
import           Data.Text (Text)
import qualified Data.Time.Clock as Clock
import qualified Data.Tuple as Tuple
import qualified Numeric
import qualified System.Directory as Directory
import qualified System.FilePath as FilePath
import           System.FilePath ((</>))

import qualified Language.Preprocessor.Cpphs as Cpphs

import qualified FixImports.Config as Config
import qualified FixImports.Format as Format
import qualified FixImports.Index as Index
import qualified FixImports.Parse as Parse
import qualified FixImports.Types as Types
import qualified FixImports.Util as Util

import Control.Monad


-- | Look only this deep in the directory hierarchy for local modules.
searchDepth :: Int
searchDepth = 12

data Result = Result {
    resultRange :: (Row, Row)
    , resultImports :: String
    , resultAdded :: Set Types.ModuleName
    , resultRemoved :: Set Types.ModuleName
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
    result <- parse (Config._language config) modulePath processedSource
    case result of
        Left err -> return (Left err, [])
        Right (mod, cmts) -> do
            mParse <- metric (mod `seq` (), cmts) "parse"
            index <- Index.load
            mLoad <- metric () "load-index"
            let extracted = extract config mod cmts
            mExtract <- metric extracted "extract"
            case checkForCpp (_importRange extracted) source of
                [] -> fmap (fmap List.reverse) $ flip State.runStateT [] $
                    fmap (addMetrics [mStart, mCpp, mParse, mLoad, mExtract]) <$>
                    fixImports ioFilesystem config index modulePath extracted
                cpps -> return
                    ( Left $ "can't handle CPP directives in import block:\n"
                        <> unlines cpps
                    , []
                    )

parse :: [Types.Extension] -> FilePath -> String
    -> IO (Either String (Parse.Module, [Parse.Comment]))
parse extensions modulePath = Parse.parse extensions modulePath

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

-- | I have to get the CPP out before parsing and fixing imports, but then it's
-- hard to put it back in again.  Especially the main reason for CPP is
-- conditional imports, which means I might not even know what to do with them.
-- I suppose I could try to detect them and preserve them, but for now it's
-- simpler to just abort on any CPP.  At least it's better than silently
-- deleting it.
checkForCpp :: (Row, Row) -> String -> [String]
checkForCpp (start, end) =
    map (\(i, line) -> show i <> ":" <> line)
    . filter (any (`Set.member` cppThings) . words . snd)
    . take (end-start) . drop start . zip [1 :: Int ..] . lines
    where
    cppThings = Set.fromList $ map ("#"<>)
        [ "define", "undef", "include", "if", "ifdef", "ifndef", "else"
        , "elif", "endif", "line", "error", "pragma"
        ]

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
        Util.partitionM isDir fns
    , _doesFileExist = Directory.doesFileExist
    , _metric = metric
    }
    where
    -- Symlinks are not directories, so I don't walk into them.
    isDir fn = (&&) <$> Directory.doesDirectoryExist fn
        <*> (not <$> Directory.pathIsSymbolicLink fn)

type LogT m a = State.StateT [Text] m a

debug :: Monad m => Config.Config -> Text -> LogT m ()
debug config msg = when (Config._debug config) $ State.modify' (msg:)
    -- The check is unnecessary since I check debug before printing them, but
    -- it'll save a thunk at least.

-- | Take a parsed module along with its unparsed text.  Generate a new import
-- block with proper spacing, formatting, and comments.  Then snip out the
-- import block on the import file, and replace it.
fixImports :: Monad m => Filesystem m -> Config.Config -> Index.Index
    -> FilePath -> Extracted -> LogT m (Either String Result)
fixImports fs config index modulePath extracted = do
    mbNew <- mapM (findNewImport fs config modulePath index)
        (Set.toList (_missingImports extracted))
    mNewImports <- lift $ _metric fs mbNew "find-new-imports"
    (imports, newUnqualImports, unusedUnqual) <- return $
        fixUnqualified (_modToUnqualifieds extracted) config
            (_unchangedImports extracted)
    newUnqualImports <- lift $ mapM (locateImport fs) newUnqualImports
    mUnqual <- lift $ _metric fs newUnqualImports "unqualified-imports"
    mbExisting <- mapM (findImport fs index (Config._includes config)) imports
    mExistingImports <- lift $ _metric fs mbExisting "find-existing-imports"
    let existing = map (Types._importName . fst) imports
    let (notFound, importLines) = Either.partitionEithers $
            zipWith mkError
                (map qualToMod (Set.toList (_missingImports extracted))
                    ++ existing)
                (mbNew ++ mbExisting)
        mkError _ (Just imp) = Right imp
        mkError mod Nothing = Left mod
    let formattedImports =
            Format.formatGroups (Config._format config) (Config._order config)
                (importLines ++ newUnqualImports)
    return $ case notFound of
        _ : _ -> Left $ "not found: "
            ++ Util.join ", " (map Types.moduleName notFound)
        [] -> Right $ Result
            { resultRange = _importRange extracted
            , resultImports = formattedImports
            , resultAdded = Set.fromList $
                map (Types._importName . Types.importDecl) $
                Maybe.catMaybes mbNew ++ newUnqualImports
            , resultRemoved =
                _unusedImports extracted <> Set.fromList unusedUnqual
            , resultMetrics = [mNewImports, mExistingImports, mUnqual]
            }
    where
    qualToMod (Types.Qualification name) = Types.ModuleName name

type ImportComment = (Types.Import, [Types.Comment])

locateImport :: Monad m => Filesystem m -> ImportComment -> m Types.ImportLine
locateImport fs (decl, cmts) = do
    isLocal <- _doesFileExist fs $ Types.moduleToPath $ Types._importName decl
    return $ Types.ImportLine
        { importDecl = decl
        , importComments = cmts
        , importSource = if isLocal then Types.Local else Types.Package
        }


-- | Add unqualified imports.
--
-- - Get unqualifieds.
-- - If _unqualified non-empty, filter them to the ones in _unqualified.
-- - Add or modify import lines for them.
-- - Remove imports that don't appear in modToUnqualifieds.
fixUnqualified :: Map Types.ModuleName (Set Types.Name)
    -> Config.Config -> [ImportComment]
    -> ([ImportComment], [ImportComment], [Types.ModuleName])
    -- ^ (modified, new, removed)
fixUnqualified modToUnqualifieds config imports =
    removeEmptyImports moduleToNames $
        first (map (first stripReferences)) $
        foldr addReferences (imports, []) $
        Map.toList modToUnqualifieds
    where
    -- Remove managed unqualified imports that are no longer referenced.
    stripReferences :: Types.Import -> Types.Import
    stripReferences imp = Types.importModify (filter keep) imp
        where
        moduleName = Types._importName imp
        -- Keep if it's not managed, or it is managed and referenced.
        keep (Types.Entity Nothing var Nothing) =
            not (isManaged moduleName var)
            || maybe False (var `Set.member`)
                (Map.lookup moduleName modToUnqualifieds)
        keep _ = True
    -- Successively modify the ImportComment list for each new reference.  Keep
    -- existing modified imports separate from newly added ones, so they can be
    -- reported as adds.
    addReferences :: (Types.ModuleName, Set Types.Name)
        -> ([ImportComment], [ImportComment])
        -> ([ImportComment], [ImportComment])
    addReferences (moduleName, names) (existing, new) =
        case Util.modifyAt (matches moduleName . fst) add existing of
            Nothing -> (existing, (newImport, []) : new)
            Just modified -> (modified, new)
        where
        add = first $ Types.importModify (newEntities ++)
        newImport = (Types.makeImport moduleName)
            { Types._importEntities = Just $ map Right newEntities }
        newEntities = map mkEntity $ Set.toList names
    matches name imp = Types.importUnqualified imp
        && Types._importName imp == name

    isManaged moduleName name = maybe False (name `elem`) $
        Map.lookup moduleName moduleToNames
    moduleToNames :: Map Types.ModuleName [Types.Name]
    moduleToNames = Util.multimap . map Tuple.swap . Map.toList
        . Config._unqualified $ config
    mkEntity var = Types.Entity Nothing var Nothing

-- | Remove unqualified imports that have been made empty.
removeEmptyImports :: Map Types.ModuleName names -> ([ImportComment], new)
    -> ([ImportComment], new, [Types.ModuleName])
removeEmptyImports moduleToNames (modified, new) =
    (kept, new, map (Types._importName . fst) removed)
    where
    (kept, removed) = List.partition (not . emptyImport . fst) modified
    emptyImport imp = Map.member (Types._importName imp) moduleToNames
        -- Can delete if it has an import list, but it's empty.
        && Types.importEmpty imp


-- | Make a map from each module with unqualified imports to its unqualified
-- imports that occur in the module.
makeModToUnqualifieds :: Config.Config -> Parse.Module
    -> Map Types.ModuleName (Set Types.Name)
makeModToUnqualifieds config mod
    | unqual == mempty = mempty
    | otherwise = Util.setmap $
        Maybe.mapMaybe (\name -> (, name) <$> Map.lookup name unqual) $
        Set.toList $ Parse.unqualifieds mod
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
        { importDecl = Types.setQualification qual (Types.makeImport mod)
        , importComments = []
        , importSource = source
        }

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
findImport :: Monad m => Filesystem m -> Index.Index -> [FilePath]
    -> ImportComment -> LogT m (Maybe Types.ImportLine)
findImport fs index includes (imp, cmts) = do
    found <- findModuleName fs index includes (Types._importName imp)
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

-- | All the relevant info extracted from a module.
data Extracted = Extracted {
    -- | References exist, but no corresponding imports.
    _missingImports :: Set Types.Qualification
    -- | Imports exist but no reference.
    , _unusedImports :: Set Types.ModuleName
    , _unchangedImports :: [ImportComment]
    , _importRange :: (Int, Int)
    , _modToUnqualifieds :: Map Types.ModuleName (Set Types.Name)
    }

instance DeepSeq.NFData Extracted where
    rnf (Extracted a b c d e) = DeepSeq.rnf (a, b, c, d, e)

extract :: Config.Config -> Parse.Module -> [Parse.Comment] -> Extracted
extract config mod cmts = Extracted
    { _missingImports = missing
    , _unusedImports = unused
    , _unchangedImports = importCmts
    , _importRange = range
    , _modToUnqualifieds = makeModToUnqualifieds config mod
    }
    where
    unused = Set.difference (Set.fromList modules)
        (Set.fromList (map (Types._importName . fst) importCmts))
    missing = Set.difference used imported
    -- If the Prelude isn't explicitly imported, it's implicitly imported, so
    -- if I see Prelude.x it doesn't mean to add an import.
    imported = Set.fromList $ prelude : qualifiedImports
    importCmts =
        [ impCmt
        | impCmt <- associateComments imports $
            dropWhile before $ List.sort cmts
        , keepImport (fst impCmt)
        ]
    before = (< fst range) . Types._startLine . Parse._span
    range = Parse.importRange mod
    -- Keep unqualified imports, but only keep qualified ones if they are used.
    -- Prelude is considered always used if it appears, because removing it
    -- changes import behavour.
    keepImport imp =
        Set.member (Types.importQualification imp) (Set.insert prelude used)
        || not (Types._importQualified imp)
    prelude = Types.Qualification "Prelude"

    -- Get from the qualified import name back to the actual module name so
    -- I can return that.
    modules = map Types._importName imports
    used = Parse.qualifications mod
    qualifiedImports = map Types.importQualification imports
    imports = normalizeImports $ Parse.extractImports mod

-- | Clean up redundant imports.
normalizeImports :: [Types.Import] -> [Types.Import]
normalizeImports imports =
    Util.uniqueOn key qual
    ++ map merge (Util.groupOn key (Util.sortOn key unqual))
    where
    (qual, unqual) = List.partition Types._importQualified imports
    key imp = imp
        { Types._importEntities = Nothing
        , Types._importSpan = Types.noSpan
        }
    merge group@(imp:_) = imp
        { Types._importEntities = mconcat (map Types._importEntities group) }
    merge [] = error "groupOn postcondition"

-- | Pair Imports up with the comments that apply to them.  Comments
-- below the last import are dropped, but there shouldn't be any of those
-- because they should have been omitted from the comment block.
--
-- Spaces between comments above an import will be lost, and multiple comments
-- to the right of an import (e.g. commenting a complicated import list) will
-- probably be messed up.  TODO Fix it if it becomes a problem.
associateComments :: [Types.Import] -> [Parse.Comment] -> [ImportComment]
associateComments imports cmts = snd $ List.mapAccumL associate cmts imports
    where
    associate cmts imp = (after, (imp, associated))
        where
        associated = map (Types.Comment Types.CmtAbove . Parse._comment) above
            ++ map (Types.Comment Types.CmtRight . Parse._comment) right
        -- cmts that end before the import beginning are above it
        (above, rest) = List.span ((< start impSpan) . end . Parse._span) cmts
        -- remaining cmts that start before or at the import's end are right
        -- of it
        (right, after) = List.span ((<= end impSpan) . start . Parse._span) rest
        impSpan = Types._importSpan imp
    start = Types._startLine
    end = Types._endLine

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
