-- | Maintain the index from Qualification to the full module from the package
-- db that this Qualification probably intends.
module Index where
import Prelude hiding (mod)
import Control.Applicative ( (<$>) )
import Control.Monad.Instances () -- Monad (Either x) instance
import qualified Data.List as List
import qualified Data.Map as Map
import qualified System.Process as Process

import qualified Types
import qualified Util


data Config = Config {
    -- | The modules in the packages earlier in this list will override those
    -- later.  So even if there's a package with @Some.Obscure.List@, @List@
    -- will still map to @Data.List@ because @containers@ gets priority.
    --
    -- The rest will be in a consistent but uninteresting order.
    configPackagePriorities :: [String]

    -- | Ignore these packages entirely even if they are exposed, in case they
    -- export something annoyingly general.
    , configIgnorePackages :: [String]
    } deriving (Show)

-- | Map from tails of the each module in the package db to its module name.
-- So @List@ and @Data.List@ will map to @Data.List@.  Modules from a set
-- of core packages, like base and containers, will take priority, so even if
-- there's a package with @Some.Obscure.List@, @List@ will still map to
-- @Data.List@.
type Index = Map.Map Types.Qualification Types.ModuleName

empty :: Index
empty = Map.empty

loadIndex :: Config -> IO Index
loadIndex = buildIndex -- TODO load cache?

buildIndex :: Config -> IO Index
buildIndex config = do
    result <- parseDump config <$> Process.readProcess "ghc-pkg"
        ["field", "*", "name,exposed,exposed-modules"] ""
    either error return result

parseDump :: Config -> String -> Either String Index
parseDump config text = do
    triples <- sequence (triple sections)
    return $ Map.fromList $ makePairs (configPackagePriorities config)
        [(package, mods) | (package, True, mods) <- triples,
            package `notElem` configIgnorePackages config]
    where
    sections = List.unfoldr parseSection (lines text)
    triple [] = []
    triple (("name", [name]) : ("exposed", [exposed])
            : ("exposed-modules", modules) : rest) =
        Right (name, exposed == "True", map Types.ModuleName modules)
            : triple rest
    triple ((tag, _) : _) = [Left $ "unexpected tag: " ++ tag]

makePairs :: [String] -> [(String, [Types.ModuleName])]
    -> [(Types.Qualification, Types.ModuleName)]
makePairs packagePrio packageMods =
    [(qual, mod) | (_, mod) <- pairs, qual <- modQuals mod]
    where
    pairs = Util.sortOn (priority (reverse packagePrio))
        [(p, m) | (p, mods) <- packageMods, m <- mods]
    modQuals = map (Types.Qualification . Util.join ".") . filter (not . null)
        . List.tails . Util.split "." . Types.moduleName

-- | Put the high priority packages last, so they will override the others
-- in @Map.fromList@.
priority :: [String] -> (String, Types.ModuleName) -> (Int, Int, String)
priority packagePrio (package, mod) = (pprio, mprio, package)
    where
    pprio = maybe 0 (+1) (List.elemIndex package packagePrio)
    mprio = negate $ length $ filter (=='.') $ Types.moduleName mod

parseSection :: [String] -> Maybe ((String, [String]), [String])
parseSection [] = Nothing
parseSection (x:xs) = Just ((tag, concatMap words (drop 1 rest : pre)), post)
    where
    (tag, rest) = break (==':') x
    (pre, post) = span ((==" ") . take 1) xs

{-
-- * test

t0 = makePairs prio $ map (\(p, ms) -> (p, map Types.ModuleName ms))
    [ ("base", ["Data.List", "Foo.Bar.List"])
    , ("haskell98", ["List"])
    , ("containers", ["Box.List"])
    ]
    -- Not containers Box.List because containers is lower prio.
    -- Not haskell98 List because haskell98 is lowest prio, even though that's
    -- the shortest match.
    -- Not Foo.Bar.List because Data.List is a shorter match.

t1 = Map.lookup (Types.Qualification "List") (Map.fromList t0)
t10 = List.unfoldr parseSection ["hi: there fred", "  foo", "next: section"]
tdump =
    [ "name: base", "exposed: True", "exposed-modules: Data.List Data.Map"
    , "name: mtl", "exposed: True", "exposed-modules: Monad.B.List"
    ]
-}
