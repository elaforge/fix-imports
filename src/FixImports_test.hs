{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
module FixImports_test where
import Control.Monad (unless, void)
import qualified Control.Monad.Identity as Identity
import Data.Bifunctor (bimap)
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Time.Clock.POSIX as Clock.POSIX

import qualified Language.Haskell.Exts as Haskell
import qualified System.FilePath as FilePath

import qualified Config
import qualified FixImports
import qualified Index
import qualified Types

import EL.Test.Global


test_simple = do
    let run config = fmap FixImports.resultText
            . fixModule index [] (mkConfig config) "A.hs"
        index = Index.makeIndex
            [ ("pkg", ["A.B"])
            ]
    equal (run "" "x = B.c") $ Right
        "import qualified A.B as B\n\
        \x = B.c\n"
    equal (run "" "x = A.B.c") $ Right
        "import qualified A.B\n\
        \x = A.B.c\n"
    leftLike (run "" "x = Z.c") "not found: Z"

fixModule :: Index.Index -> [FilePath]
    -> Config.Config -> FilePath -> String -> Either String FixImports.Result
fixModule index files config modulePath source =
    case FixImports.parse (Config._language config) modulePath source of
        Haskell.ParseFailed srcloc err ->
            Left $ Haskell.prettyPrint srcloc ++ ": " ++ err
        Haskell.ParseOk (mod, cmts) -> Identity.runIdentity $
            FixImports.fixImports (pureFilesystem files) config index
                modulePath mod cmts source

pureFilesystem :: [FilePath] -> FixImports.Filesystem Identity.Identity
pureFilesystem files = FixImports.Filesystem
    { _listDir = return . Maybe.fromMaybe ([], []) . (`Map.lookup` tree)
    , _doesFileExist = return . (`elem` files)
    , _metric = \_ ->
        return (Clock.POSIX.posixSecondsToUTCTime 0, Text.pack "metric")
    }
    where tree = filesToTree files

-- group by first element, then second, etc.
filesToTree :: [FilePath] -> Map.Map FilePath ([FilePath], [FilePath])
    -- ^ (dirs, files)
filesToTree = Map.fromList
    . map (bimap (List.dropWhileEnd (=='/')) separate) . groupFst
    . concatMap prefixes
    where
    separate [""] = ([], [])
    separate subs = (unique $ map (takeWhile (/='/')) dirs, files)
        where (dirs, files) = List.partition ('/' `elem`) subs

prefixes :: FilePath -> [(FilePath, FilePath)]
prefixes = map (bimap concat concat) . drop 1
    . (\xs -> zip (List.inits xs) (List.tails xs)) . FilePath.splitPath

mkConfig :: Text.Text -> Config.Config
mkConfig content
    | null errs = config
    | otherwise = error $ "parsing " <> show content  <> ": "
        <> unlines (map Text.unpack errs)
    where (config, errs) = Config.parse content

tmod0 :: String
tmod0 =
    "-- cmt1\n\
    \-- cmt2\n\
    \import Data.List\n\
    \x = 42\n"

tmod1 = "module Foo\nwhere\n" ++ tmod0
tmod2 = "module Foo\nwhere\nx = 42\n"

tmod :: String
tmod =
    "{-# LANGUAGE SomeExtension #-} -- comment on LANGUAGE\n\
    \-- cmt1 for Data.List\n\
    \-- cmt2 for Data.List\n\
    \import qualified Data.List as C\n\
    \-- cmt1 for Util\n\
    \-- cmt2 for Util\n\
    \import qualified Util -- cmt right of Util\n\
    \import qualified Extra as Biz {- block cmt -}\n\
    \import Data.Map (a,\n\
    \       b)\n\
    \\n\
    \f :: Util.One -> Midi.New.Two -> Util.Foo -> C.Result\n\
    \f x = x * C.z\n"

-- * util

type NonNull a = [a]

-- | Similar to 'keyedGroupSort', but key on the fst element, and strip the
-- key out of the groups.
groupFst :: Ord a => [(a, b)] -> [(a, NonNull b)]
groupFst xs = [(key, map snd group) | (key, group) <- keyedGroupSort fst xs]

-- | Group the unsorted list into @(key x, xs)@ where all @xs@ compare equal
-- after @key@ is applied to them.
keyedGroupSort :: Ord key => (a -> key) -> [a] -> [(key, NonNull a)]
    -- ^ Sorted by key. The NonNull group is in the same order as the input.
keyedGroupSort key = Map.toAscList . foldr go Map.empty
    where go x = Map.alter (Just . maybe [x] (x:)) (key x)

unique :: Ord a => [a] -> [a]
unique = Set.toList . Set.fromList
