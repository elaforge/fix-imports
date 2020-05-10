{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
module FixImports_test where
import Control.Monad (unless, void)
import qualified Control.Monad.Identity as Identity
import qualified Control.Monad.State.Strict as State
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
    let run config = fmap FixImports.resultImports
            . fixModule index ["C.hs"] (mkConfig config) "A.hs"
        index = Index.makeIndex
            [ ("pkg", ["A.B"])
            , ("zpkg", ["Z"])
            ]
    equal (run "" "x = B.c") $ Right "import qualified A.B as B\n"
    equal (run "" "x = A.B.c") $ Right "import qualified A.B\n"
    leftLike (run "" "x = Q.c") "not found: Q"
    -- Remove unused.
    equal (run "" "import qualified A.B as B\n\nx = y") $ Right ""
    -- Unless it's unqualified.
    equal (run "" "import A.B as B\n\nx = y") $ Right
        "import A.B as B\n"

    -- Local goes below package.
    equal (run "" "x = (B.a, C.a, Z.a)") $ Right
        "import qualified A.B as B\n\
        \import qualified Z\n\
        \\n\
        \import qualified C\n"

    -- Don't mess with imports I don't manage.
    equal (run "" "import A.B hiding (mod)\n") $
        Right "import A.B hiding (mod)\n"
    equal (run "" "import A.B\n") $
        Right "import A.B\n"

test_qualifyAs = do
    let run config files = fmap eResult . fixModule index files config "A.hs"
        config = mkConfig "qualify-as: Data.Text.Lazy as DTL"
        index = Index.makeIndex [("text", ["Data.Text.Lazy"])]
    equal (run config [] "x = DTL.y") $ Right
        ( ["Data.Text.Lazy"]
        , []
        , "import qualified Data.Text.Lazy as DTL\n"
        )
    equal (run config [] "import qualified Data.Text.Lazy as DTL\n") $ Right
        ( []
        , ["Data.Text.Lazy"]
        , ""
        )

    -- qualifyAs aliases are prioritized in the same way as others, so
    -- the local module wins:
    equal (run config ["DTL.hs"] "x = DTL.y") $
        Right (["DTL"], [], "import qualified DTL\n")

    -- Unless explicitly suppressed:
    let config2 = mkConfig $ Text.unlines
            [ "qualify-as: Data.Text.Lazy as DTL"
            , "prio-module-high: Data.Text.Lazy"
            ]
    equal (run config2 ["DTL.hs"] "x = DTL.y") $ Right
        ( ["Data.Text.Lazy"]
        , []
        , "import qualified Data.Text.Lazy as DTL\n"
        )

test_unqualified = do
    let run config = fmap eResult
            . fixModule index ["C.hs"] (mkConfig config) "A.hs"
        index = Index.makeIndex
            [ ("pkg", ["A.B"])
            , ("zpkg", ["Z"])
            ]
    equal (run "unqualified: A.B (c)" "x = (c, c)") $ Right
        ( ["A.B"]
        , []
        , "import A.B (c)\n"
        )

    -- Modify an existing import.
    equal (run "unqualified: A.B (c)" "import A.B (a, z)\nx = (c, c)") $ Right
        ( []
        , []
        , "import A.B (a, c, z)\n"
        )
    equal (run "unqualified: A.B (a, c)" "import A.B (a, c)\nx = a") $ Right
        ( []
        , []
        , "import A.B (a)\n"
        )
    -- Don't accumulate duplicates.
    equal (run "unqualified: A.B (c)" "import A.B (c)\nx = c") $
        Right ([], [], "import A.B (c)\n")
    equal (run "unqualified: A.B (C)" "import A.B (C)\nx :: C") $
        Right ([], [], "import A.B (C)\n")

    -- Don't manage it if it's not mine.
    equal (run "unqualified: A.B (c)" "import A.B (d)") $ Right
        ([], [], "import A.B (d)\n")

    -- local still goes below package
    equal (run "unqualified: C (a)" "import A.B\nimport Z\nx = a") $ Right
        ( ["C"]
        , []
        , "import A.B\n\
          \import Z\n\
          \\n\
          \import C (a)\n"
        )

    -- Don't import when it's an assignee.
    equal (run "unqualified: A.B (c)" "c = x") $ Right ([], [], "")
    equal (run "unqualified: A.B ((</>))" "x = a </> b") $
        Right (["A.B"], [], "import A.B ((</>))\n")

    -- Removed unused.
    equal (run "unqualified: A.B (c)" "import A.B (c)\nx = x\n") $
        Right ([], ["A.B"], "")
    -- But not if it's a spec-less import.
    equal (run "unqualified: A.B (c)" "import A.B\nx = x\n") $
        Right ([], [], "import A.B\n")

eResult :: FixImports.Result -> ([Types.ModuleName], [Types.ModuleName], String)
eResult r =
    ( Set.toList (FixImports.resultAdded r)
    , Set.toList (FixImports.resultRemoved r)
    , FixImports.resultImports r
    )

fixModule :: Index.Index -> [FilePath]
    -> Config.Config -> FilePath -> String -> Either String FixImports.Result
fixModule index files config modulePath source =
    case FixImports.parse (Config._language config) modulePath source of
        Haskell.ParseFailed srcloc err ->
            Left $ Haskell.prettyPrint srcloc ++ ": " ++ err
        Haskell.ParseOk (mod, cmts) ->
            fst $ Identity.runIdentity $ flip State.runStateT [] $
            FixImports.fixImports (pureFilesystem files) config index
                modulePath mod cmts

-- | The ./ stuff is tricky, this is probably still wrong.
pureFilesystem :: [FilePath] -> FixImports.Filesystem Identity.Identity
pureFilesystem files = FixImports.Filesystem
    { _listDir = \dir -> return
        . Maybe.fromMaybe ([], []) . (`Map.lookup` tree) $ dir
    , _doesFileExist = \fn -> return . (`elem` files) . normalize $ fn
    , _metric = \_ _ ->
        return (Clock.POSIX.posixSecondsToUTCTime 0, Text.pack "metric")
    }
    where
    tree = filesToTree (map ("./"++) files)
    normalize ('.':'/':fn) = fn
    normalize fn = fn

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
    | null errs = config { Config._includes = ["."] }
    | otherwise = error $ "parsing " <> show content  <> ": "
        <> unlines (map Text.unpack errs)
    where (config, errs) = Config.parse content

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
