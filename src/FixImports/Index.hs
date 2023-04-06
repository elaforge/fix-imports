{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
-- | Maintain the index from Qualification to the full module from the package
-- db that this Qualification probably intends.
module FixImports.Index (
    Index, Package, empty, load, showIndex, makeIndex, parseSections
) where
import Prelude hiding (mod)
import           Control.Monad
import           Data.Bifunctor (second)
import           Data.Maybe (mapMaybe)
import           Data.Text (Text)
import           System.FilePath ((</>))
import qualified Data.Either as Either
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import qualified GHC.Paths

import qualified System.Directory as Directory
import qualified System.IO as IO

import qualified FixImports.PkgCache as PkgCache
import qualified FixImports.Types as Types
import qualified FixImports.Util as Util


-- | Map from tails of the each module in the package db to its module name.
-- So @List@ and @Data.List@ will map to @Data.List@.  Modules from a set
-- of core packages, like base and containers, will take priority, so even if
-- there's a package with @Some.Obscure.List@, @List@ will still map to
-- @Data.List@.
type Index = Map.Map Types.Qualification [(Package, Types.ModuleName)]

-- | Package name without the version.
type Package = String

empty :: Index
empty = Map.empty

load :: Maybe FilePath -> IO (Index, Text)
load (Just pkgCache) = do
    unitNameModules <- PkgCache.loadCache pkgCache
    return
        ( makeIndex $ map (fmap (map Types.ModuleName)) $
            map snd unitNameModules
        , "--package-cache flag"
        )
load Nothing = fromGhcEnvironment >>= \case
    Just index -> return (index, ".ghc.environment")
    Nothing -> (, "global ghc-pkg") <$> fromGhcPkg

showIndex :: Index -> Text
showIndex index = Text.unlines
    [ Text.pack k <> ": " <> Text.pack (show v)
    | (Types.Qualification k, v) <- Map.toAscList index
    ]

-- | I think the global package db is always under the libdir?
bootPkgDb :: FilePath
bootPkgDb = GHC.Paths.libdir </> "package.conf.d"

fromGhcEnvironment :: IO (Maybe Index)
fromGhcEnvironment = parseGhcEnvironment >>= \case
    Nothing -> return Nothing
    Just (pkgDbs, unitIds) -> do
        nameModules <- PkgCache.load (Set.fromList unitIds)
            (bootPkgDb : pkgDbs)
        return $ Just $ makeIndex $
            map (fmap (map Types.ModuleName)) nameModules

-- | The code to write .ghc.environment is in Cabal
-- Distribution.Simple.GHC.Internal, the code to read it is copy pasted over
-- into cabal-install Distribution.Client.CmdInstall.  So they're not even
-- thinking of being consistent with themselves, let alone anyone else.
-- Too much bother.
parseGhcEnvironment :: IO (Maybe ([FilePath], [PkgCache.UnitId]))
parseGhcEnvironment = do
    envFiles <- filter (".ghc.environment." `List.isPrefixOf`) <$>
        Directory.listDirectory "."
    case envFiles of
        [] -> return Nothing
        [envFile] -> Just . parseEnvFile <$> Text.IO.readFile envFile
        _ -> error $ "multiple ghc env files: " <> unwords envFiles

parseEnvFile :: Text -> ([FilePath], [PkgCache.UnitId])
parseEnvFile = Either.partitionEithers . mapMaybe parse . Text.lines
    where
    parse line = case Text.words line of
        ["package-db", path] -> Just $ Left $ Text.unpack path
        ["package-id", unit] -> Just $ Right unit
        _ -> Nothing
    -- clear-package-db
    -- global-package-db
    -- package-db /Users/elaforge/.cabal/store/ghc-9.2.5/package.db
    -- package-db dist-newstyle/packagedb/ghc-9.2.5
    -- package-id hlibgit2-0.18.0.16-inplace
    -- package-id base-4.16.4.0
    -- package-id bndngs-DSL-1.0.25-d82df022

fromGhcPkg :: IO Index
fromGhcPkg = do
    (_, out, err) <- Util.readProcessWithExitCode "ghc-pkg"
        ["field", "*", "name,exposed,exposed-modules"]
    unless (Text.null err) $
        IO.hPutStrLn IO.stderr $ "stderr from ghc-pkg: " ++ Text.unpack err
    let (errors, index) = parseDump out
    unless (null errors) $
        IO.hPutStrLn IO.stderr $ "errors parsing ghc-pkg output: "
            ++ List.intercalate ", " errors
    return index

makeIndex :: [(Text, [Types.ModuleName])] -- ^ [(package, modules)]
    -> Index
makeIndex packages = Map.fromListWith (++)
    [ (qual, [(Text.unpack package, mod)])
    | (package, modules) <- packages
    , mod <- modules
    , qual <- moduleQualifications mod
    ]

parseDump :: Text -> ([String], Index)
parseDump text = (errors, makeIndex packages)
    where
    (errors, packages) = Either.partitionEithers $
        extractSections (parseGhcPkg text)

extractSections :: [(Text, [Text])]
    -> [Either String (Text, [Types.ModuleName])]
extractSections = Maybe.mapMaybe extract . Util.splitWith ((=="name") . fst)
    where
    extract [ ("name", [name])
            , ("exposed", [exposed])
            , ("exposed-modules", modules)
            ]
        | exposed /= "True" = Nothing
        | otherwise = Just $
            Right (name, map (Types.ModuleName . Text.unpack) modules)
    -- It may be missing exposed-modules, but that means I don't need it.
    extract _ = Nothing

-- | Take a module name to all its possible qualifications, i.e. its list
-- of suffixes.
moduleQualifications :: Types.ModuleName -> [Types.Qualification]
moduleQualifications = map (Types.Qualification . Util.join ".")
    . filter (not . null) . List.tails . Util.split "." . Types.moduleName

parseGhcPkg :: Text -> [(Text, [Text])]
parseGhcPkg = map (second (map uncomma . concatMap Text.words)) . parseSections
    where
    -- Somewhere in 9.2, ghc-pkg switched from space separated to comma
    -- separated.
    uncomma t = Maybe.fromMaybe t (Text.stripSuffix "," t)

parseSections :: Text -> [(Text, [Text])] -- ^ [(section_name, lines)]
parseSections = List.unfoldr parseSection . stripComments . Text.lines

stripComments :: [Text] -> [Text]
stripComments =
    filter (not . Text.null) . map (Text.stripEnd . fst . Text.breakOn "--")

-- | Consume a "tag: xyz" plus indents until the next dedented section.
parseSection :: [Text] -> Maybe ((Text, [Text]), [Text])
parseSection [] = Nothing
parseSection (x:xs) = Just
    ( (tag, map Text.strip (Text.drop 1 rest : pre))
    , post
    )
    where
    (tag, rest) = Text.break (==':') x
    (pre, post) = span (" " `Text.isPrefixOf`) xs


-- * read cache


