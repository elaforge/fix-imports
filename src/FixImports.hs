{- | Automatically fix the import list in a haskell module.

    This only really works for qualified names.  The process is as follows:

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

    - If there are no imports to be added or removed, the file is returned
    unchanged.  This means it won't sort the imports in this case.
-}
module FixImports where
import Prelude hiding (mod)
import qualified Control.Monad as Monad
import qualified Data.Either as Either
import qualified Data.Generics.Uniplate.Data as Uniplate
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set

import qualified Language.Haskell.Exts.Annotated as Haskell

import qualified System.Directory as Directory
import qualified System.Environment
import qualified System.Exit
import qualified System.FilePath as FilePath
import System.FilePath ( (</>) )
import qualified System.IO as IO
import qualified System.Process as Process

import qualified Config
import qualified Index
import qualified Types
import qualified Util


runMain :: Config.Config -> IO ()
runMain config = do
    -- I need the module path to search for modules relative to it first.  I
    -- could figure it out from the parsed module name, but a main module may
    -- not have a name.
    (modulePath, verbose) <- parseArgs =<< System.Environment.getArgs
    text <- IO.getContents
    fixed <- fixModule config modulePath text
    case fixed of
        Left err -> do
            IO.putStr text
            IO.hPutStrLn IO.stderr $ "error: " ++ err
            System.Exit.exitFailure
        Right (Result text added removed) -> do
            IO.putStr text
            Monad.when (verbose
                    && (not (Set.null added) || not (Set.null removed))) $
                IO.hPutStrLn IO.stderr $ "added: " ++ names added
                    ++ "; removed: " ++ names removed
            System.Exit.exitSuccess
    where
    names xs
        | Set.null xs = "[]"
        | otherwise = (Util.join ", " . map Types.moduleName . Set.toList) xs

parseArgs :: [String] -> IO (String, Bool)
parseArgs args = case filter (/="-v") args of
        [modulePath] -> return (modulePath, verbose)
        _ -> do
            IO.hPutStrLn IO.stderr $
                "usage: FixImports [ -v ] Module.hs <Module.hs"
            System.Exit.exitFailure
    where
    verbose = "-v" `elem` args

data Result = Result {
    resultText :: String
    , resultAdded :: Set.Set Types.ModuleName
    , resultRemoved :: Set.Set Types.ModuleName
    } deriving (Show)

fixModule :: Config.Config -> FilePath -> String -> IO (Either String Result)
fixModule config modulePath text = case parse text of
    Haskell.ParseFailed srcloc err ->
        return $ Left $ Haskell.prettyPrint srcloc ++ ": " ++ err
    Haskell.ParseOk (mod, cmts) -> fixImports config modulePath mod cmts text
    where
    parse = Haskell.parseFileContentsWithComments
        (Haskell.defaultParseMode { Haskell.parseFilename = modulePath })

-- | Take a parsed module along with its unparsed text.  If the imports should
-- change, generate a new import block with proper spacing, formatting, and
-- comments.  Then snip out the import block on the import file, and replace
-- it.  Otherwise, the unparsed input module is returned unchanged.
fixImports :: Config.Config -> FilePath -> Types.Module -> [Haskell.Comment]
    -> String -> IO (Either String Result)
fixImports config modulePath mod cmts text
    | Set.null newImports && Set.null unusedImports =
        return $ Right $ Result text Set.empty Set.empty
    | otherwise = do
        -- Don't bother loading the index if I'm not going to use it.
        -- TODO actually, only load it if I don't find local imports
        -- I guess Data.Binary's laziness will serve me there
        index <- if Set.null newImports
            then return Index.empty
            else Index.loadIndex (Config.configIndex config)
        mbNew <- mapM (mkImportLine modulePath index) (Set.toList newImports)
        mbExisting <- mapM findImport imports
        let existing = map (Types.importDeclModule . fst) imports
        let (notFound, importLines) = Either.partitionEithers $
                zipWith mkError
                    (map toModule (Set.toList newImports) ++ existing)
                    (mbNew ++ mbExisting)
            mkError _ (Just imp) = Right imp
            mkError mod Nothing = Left mod
        return $ case notFound of
            _ : _ -> Left $ "modules not found: "
                ++ Util.join ", " (map Types.moduleName notFound)
            [] -> Right $ Result
                (substituteImports (showImports importLines) range text)
                (Set.fromList (map (Types.importDeclModule . Types.importDecl)
                    (Maybe.catMaybes mbNew)))
                unusedImports
    where
    (newImports, unusedImports, imports, range) = importInfo mod cmts
    toModule (Types.Qualification name) = Types.ModuleName name
    showImports = Config.configShowImports config

-- | Clip out the range from the given text and replace it with the given
-- lines.
substituteImports :: String -> (Int, Int) -> String -> String
substituteImports imports (start, end) text =
    unlines pre ++ imports ++ unlines post
    where
    (pre, within) = splitAt start (lines text)
    (_, post) = splitAt (end-start) within

-- * find new imports

-- | Make a new ImportLine from a ModuleName.
mkImportLine :: FilePath -> Index.Index -> Types.Qualification
    -> IO (Maybe Types.ImportLine)
mkImportLine modulePath index qual@(Types.Qualification name) = do
    found <- findModule index modulePath qual
    return $ case found of
        Nothing -> Nothing
        Just (mod, local) -> Just (Types.ImportLine (mkImport mod) [] local)
    where
    mkImport (Types.ModuleName mod) =
        Haskell.ImportDecl empty (Haskell.ModuleName empty mod)
            True False Nothing (importAs mod) Nothing
    importAs mod
        | name == mod = Nothing
        | otherwise = Just $ Haskell.ModuleName empty name
    empty = Haskell.noInfoSpan (Haskell.SrcSpan "" 0 0 0 0)

-- | Find the qualification and its ModuleName and True if it was a local
-- import.  Nothing if it wasn't found at all.
findModule :: Index.Index -> FilePath -> Types.Qualification
    -> IO (Maybe (Types.ModuleName, Bool))
findModule index modulePath qual = do
    found <- findLocalModule modulePath qual
    return $ case found of
        Just path -> Just (pathToModule path, True)
        Nothing -> case Map.lookup qual index of
            Just mod -> Just (mod, False)
            Nothing -> Nothing

-- | Given A.B, look for A/B.hs, */A/B.hs, */*/A/B.hs, etc.  Look in the
-- directory of the current module first.
findLocalModule :: FilePath -> Types.Qualification -> IO (Maybe String)
findLocalModule modulePath (Types.Qualification name) = do
    found <- findFile path 0 dir
    maybe (findFile path 4 ".") (return . Just) found
    where
    path = moduleToPath (Types.ModuleName name)
    dir = FilePath.takeDirectory modulePath

findFile :: FilePath -> Int -> FilePath -> IO (Maybe FilePath)
findFile file depth dir = fmap (fmap FilePath.normalise) $
    Util.ifM (Directory.doesFileExist current) (return (Just current)) $
        if depth <= 0 then return Nothing else descend
    where
    descend = do
        subdirs <- Monad.filterM Directory.doesDirectoryExist
            =<< Util.listDir dir
        Util.untilJust (findFile file (depth-1)) subdirs
    current = dir </> file


-- * figure out existing imports

-- | Make an existing import into an ImportLine by finding out if it's a local
-- module or a package module.
findImport :: (Types.ImportDecl, [Types.Comment]) -> IO (Maybe Types.ImportLine)
findImport (imp, cmts) = do
    found <- findModuleName (Types.importDeclModule imp)
    return $ case found of
        Nothing -> Nothing
        Just local -> Just $ Types.ImportLine imp cmts local

-- | True if it was found in a local directory, False if it was found in the
-- ghc package db, and Nothing if it wasn't found at all.
findModuleName :: Types.ModuleName -> IO (Maybe Bool)
findModuleName mod =
    Util.ifM (isLocalModule mod) (return (Just True)) $
        Util.ifM (isPackageModule mod) (return (Just False))
            (return Nothing)

isLocalModule :: Types.ModuleName -> IO Bool
isLocalModule = Directory.doesFileExist . moduleToPath

isPackageModule :: Types.ModuleName -> IO Bool
isPackageModule (Types.ModuleName name) = do
    output <- Process.readProcess "ghc-pkg"
        ["--simple-output", "find-module", name] ""
    return $ not (null output)

-- * util

pathToModule :: FilePath -> Types.ModuleName
pathToModule = Types.ModuleName .
    map (\c -> if c == '/' then '.' else c) . FilePath.dropExtension

moduleToPath :: Types.ModuleName -> FilePath
moduleToPath (Types.ModuleName name) =
    map (\c -> if c == '.' then '/' else c) name ++ ".hs"

importInfo :: Types.Module -> [Haskell.Comment]
    -> (Set.Set Types.Qualification, Set.Set Types.ModuleName,
        [(Types.ImportDecl, [Types.Comment])], (Int, Int))
    -- ^ (newModules, unusedModules, moduleToImport, rangeOfImportBlock).
importInfo mod cmts = (missing, redundantModules, declCmts, (start, end))
    where
    redundant = Set.difference imported used
    missing = Set.difference used imported
    imported = Set.fromList (Maybe.catMaybes quals)

    -- Get from the qualified import name back to the actual module name so
    -- I can return that.
    modules = map Types.importDeclModule imports
    quals = map Types.importDeclQualification imports
    qualToModule =
        Map.fromList [(qual, mod) | (Just qual, mod) <- zip quals modules]
    redundantModules = Set.fromList $ Maybe.catMaybes
        [Map.lookup qual qualToModule | qual <- Set.toList redundant]

    used = Set.fromList (moduleQNames mod)
    imports = moduleImportDecls mod
    declCmts = [imp | imp@(decl, _) <- associateComments imports importCmts,
        keepImport decl]
    -- Keep unqualified imports, but only keep qualified ones if they are used.
    keepImport = maybe True (`Set.member` used) . Types.importDeclQualification

    (start, end) = importSpan mod
    importCmts = filter inRange cmts
    inRange (Haskell.Comment _ src _) = s >= start && s < end
        where s = Haskell.srcSpanStartLine src


-- | Pair ImportDecls up with the comments that apply to them.  Comments
-- below the last import are dropped.  Comments that are separated from an
-- import by a blank line will also be lost.  It could be fixed but I don't
-- think I write those comments.
--
-- Also misses cmts that have leading whitespace.  Don't do that.
-- To do it right I'd have to sort imports and cmts and pull cmts off with
-- a foldl.
associateComments :: [Types.ImportDecl] -> [Haskell.Comment]
    -> [(Types.ImportDecl, [Types.Comment])]
associateComments imports cmts = [(imp, cmtsFor imp) | imp <- imports]
    where
    cmtsFor imp = Util.mapMaybe (associate src) cmts
        where src = Haskell.srcInfoSpan (Haskell.importAnn imp)
    associate src (Haskell.Comment block csrc text)
        | start csrc == end csrc && start csrc `within` src =
            Just $ Types.Comment Types.CmtRight cmt
        | Haskell.srcSpanStartColumn csrc == 1 && end csrc + 1 == start src =
            Just $ Types.Comment Types.CmtAbove cmt
        | otherwise = Nothing
        where
        cmt = if block then "{-" ++ text ++ "-}" else "--" ++ text
    start = Haskell.srcSpanStartLine
    end = Haskell.srcSpanEndLine
    within line src = line >= start src && line <= end src

parse :: String -> (Types.Module -> [Haskell.Comment] -> a) -> Either String a
parse text f = case Haskell.parseFileContentsWithComments mode text of
    Haskell.ParseFailed srcloc err ->
        Left $ Haskell.prettyPrint srcloc ++ ": " ++ err
    Haskell.ParseOk (mod, comments) -> Right (f mod comments)
    where mode = Haskell.defaultParseMode

moduleImportDecls :: Types.Module -> [Types.ImportDecl]
moduleImportDecls (Haskell.Module _ _ _ imports _) = imports
moduleImportDecls _ = []

-- | Return half-open line range of import block, starting from (0 based) line
-- of first import to the line after the last one.
importSpan :: Types.Module -> (Int, Int)
importSpan m = case m of
        Haskell.Module _ _ _ imports@(_:_) _ ->
            (start (Haskell.importAnn (head imports)) - 1,
                end (Haskell.importAnn (last imports)))
        Haskell.Module _ (Just (Haskell.ModuleHead src _ _ _)) _ _ _ ->
            (end src, end src)
        _ -> (0, 0)
    where
    start = Haskell.srcSpanStartLine . Haskell.srcInfoSpan
    end = Haskell.srcSpanEndLine . Haskell.srcInfoSpan

-- | Uniplate is rad.
moduleQNames :: Types.Module -> [Types.Qualification]
moduleQNames mod = [Types.moduleToQualification m
    | Haskell.Qual _ m _ <- Uniplate.universeBi mod]


{-
-- * test

test = do
    res <- fixModule (Config.defaultConfig ["base"])  "TestMod.hs" tmod
    case res of
        Right res -> do
            putStr (resultText res)
            putStrLn $ "added: " ++ show (resultAdded res)
            putStrLn $ "removed: " ++ show (resultRemoved res)
        Left err -> putStrLn $ "error: " ++ err

t0 = parse tmod $ \mod _ -> moduleImportDecls mod
t1 = parse tmod $ \mod _ -> show (moduleQNames mod)
t2 = parse tmod importInfo
t3 = parse tmod $ \mod cmts ->
    [(Haskell.importModule imp, cs)
        | (imp, cs) <- associateComments (moduleImportDecls mod) cmts]

Right (mod0, cs0) = parse tmod (,)

tmod = "module TestMod (\n\
\       x, y, z\n\
\) where\n\
\import qualified Data.List as C\n\
\-- I want this comment\n\
\import qualified Util -- cmt right\n\
\import qualified Extra as Biz {- block cmt -}\n\
\import Data.Map (a,\n\
\       b)\n\
\\n\
\f :: Util.One -> Midi.New.Two -> Util.Foo -> C.Result\n\
\f x = x * C.z\n"
-}
