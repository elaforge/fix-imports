{-# LANGUAGE OverloadedStrings #-}
-- | Maintain the index from Qualification to the full module from the package
-- db that this Qualification probably intends.
module FixImports.Index (
    Index, Package, empty, load, showIndex, makeIndex, parseSections
) where
import Prelude hiding (mod)
import Control.Monad
import qualified Data.Either as Either
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Maybe as Maybe
import Data.Text (Text)

import qualified System.IO as IO

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

load :: IO Index
load = build -- TODO load cache?

showIndex :: Index -> Text
showIndex index = Text.unlines
    [ Text.pack k <> ": " <> Text.pack (show v)
    | (Types.Qualification k, v) <- Map.toAscList index
    ]

build :: IO Index
build = do
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
        extractSections (parseSections text)

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

parseSections :: Text -> [(Text, [Text])] -- ^ [(section_name, words)]
parseSections = List.unfoldr parseSection . stripComments . Text.lines

stripComments :: [Text] -> [Text]
stripComments =
    filter (not . Text.null) . map (Text.stripEnd . fst . Text.breakOn "--")

-- | Consume a "tag: name, name," plus indents until the next dedented section.
parseSection :: [Text] -> Maybe ((Text, [Text]), [Text])
parseSection [] = Nothing
parseSection (x:xs) =
    Just ((tag, concatMap Text.words (Text.drop 1 rest : pre)), post)
    where
    (tag, rest) = Text.break (==':') x
    (pre, post) = span (" " `Text.isPrefixOf`) xs
