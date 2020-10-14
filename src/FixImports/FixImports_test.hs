{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
module FixImports.FixImports_test where
import           Control.Monad (unless, void)
import qualified Control.Monad.Identity as Identity
import qualified Control.Monad.State.Strict as State
import           Data.Bifunctor (bimap)
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Time.Clock.POSIX as Clock.POSIX
import qualified System.IO.Unsafe as Unsafe

import qualified System.FilePath as FilePath

import qualified FixImports.Config as Config
import qualified FixImports.FixImports as FixImports
import qualified FixImports.Index as Index
import qualified FixImports.Types as Types

import EL.Test.Global


test_simple = do
    let run = fmap FixImports.resultImports
            . fixModule index ["C.hs"] (mkConfig "") "A.hs"
        index = Index.makeIndex
            [ ("pkg", ["A.B"])
            , ("zpkg", ["Z"])
            ]
    rightEqual (run "x = B.c") "import qualified A.B as B\n"
    rightEqual (run "x = A.B.c") "import qualified A.B\n"
    leftLike (run "x = Q.c") "not found: Q"
    -- Remove unused.
    rightEqual (run "import qualified A.B as B\n\nx = y") ""
    -- Unless it's unqualified.
    rightEqual (run "import A.B as B\n\nx = y")
        "import A.B as B\n"

    -- Local goes below package.
    rightEqual (run "x = (B.a, C.a, Z.a)")
        "import qualified A.B as B\n\
        \import qualified Z\n\
        \\n\
        \import qualified C\n"

    -- Don't mess with imports I don't manage.
    rightEqual (run "import A.B hiding (mod)\n") "import A.B hiding (mod)\n"
    rightEqual (run "import A.B\n") "import A.B\n"
    -- remove redundant imports
    rightEqual (run "import qualified Z\nimport qualified Z\nf = Z.a")
        "import qualified Z\n"

test_comments = do
    let run = fmap FixImports.resultImports
            . fixModule index [] (mkConfig "") "A.hs"
        index = Index.makeIndex [("pkg", ["A", "B"])]
    rightEqual (run "import A") "import A\n"
    -- Comments out of the edited range are not affected.
    rightEqual
        (run
            "-- before\n\
            \module M where -- where\
            \-- above\n\
            \import A\n\
            \-- below")
        "import A\n"
    rightEqual
        (run
            "module M where\n\
            \import A\n\
            \-- above1\n\
            \-- above2\n\
            \import B {- right -}\n")

        "import A\n\
        \-- above1\n\
        \-- above2\n\
        \import B {- right -}\n"

test_qualifyAs = do
    let run config files = fmap eResult . fixModule index files config "A.hs"
        config = mkConfig "qualify-as: Data.Text.Lazy as DTL"
        index = Index.makeIndex [("text", ["Data.Text.Lazy"]), ("pkg", ["A"])]
    rightEqual (run config [] "x = DTL.y")
        ( ["Data.Text.Lazy"]
        , []
        , "import qualified Data.Text.Lazy as DTL\n"
        )
    rightEqual (run config [] "import qualified Data.Text.Lazy as DTL\n")
        ( []
        , ["Data.Text.Lazy"]
        , ""
        )

    -- qualifyAs aliases are prioritized in the same way as others, so
    -- the local module wins:
    rightEqual (run config ["DTL.hs"] "x = DTL.y")
        (["DTL"], [], "import qualified DTL\n")

    -- Unless explicitly suppressed:
    let config2 = mkConfig $ Text.unlines
            [ "qualify-as: Data.Text.Lazy as DTL"
            , "prio-module-high: Data.Text.Lazy"
            ]
    rightEqual (run config2 ["DTL.hs"] "x = DTL.y")
        ( ["Data.Text.Lazy"]
        , []
        , "import qualified Data.Text.Lazy as DTL\n"
        )

    -- strip duplicates
    rightEqual (run config2 []
        "import qualified Data.Text.Lazy as DTL\n\
        \import qualified Data.Text.Lazy as DTL\n\
        \x = DTL.y")
        ([], [], "import qualified Data.Text.Lazy as DTL\n")
    -- not confused by other imports with the same name
    rightEqual (run config2 []
        "import qualified A as DTL\n\
        \import qualified Data.Text.Lazy as DTL\n\
        \x = DTL.y")
        ( []
        , []
        , "import qualified A as DTL\nimport qualified Data.Text.Lazy as DTL\n"
        )

test_unqualified = do
    let run config = fmap eResult
            . fixModule index ["C.hs"] (mkConfig ("unqualified: " <> config))
                "A.hs"
        index = Index.makeIndex
            [ ("pkg", ["A.B"])
            , ("zpkg", ["Z"])
            ]
    rightEqual (run "A.B (c)" "x = (c, c)")
        ( ["A.B"]
        , []
        , "import A.B (c)\n"
        )
    -- Modify an existing import.
    rightEqual (run "A.B (c)" "import A.B (a, z)\nx = (c, c)")
        ( []
        , []
        , "import A.B (a, c, z)\n"
        )
    rightEqual (run "A.B (a, c)" "import A.B (a, c)\nx = a")
        ( []
        , []
        , "import A.B (a)\n"
        )
    -- Don't accumulate duplicates.
    rightEqual (run "A.B (c)" "import A.B (c)\nx = c")
        ([], [], "import A.B (c)\n")
    rightEqual (run "A.B (C)" "import A.B (C)\nx :: C")
        ([], [], "import A.B (C)\n")
    -- Don't manage it if it's not mine.
    rightEqual (run "A.B (c)" "import A.B (d)")
        ([], [], "import A.B (d)\n")
    -- local still goes below package
    rightEqual (run "C (a)" "import A.B\nimport Z\nx = a")
        ( ["C"]
        , []
        , "import A.B\n\
          \import Z\n\
          \\n\
          \import C (a)\n"
        )
    -- Don't import when it's on the lhs.
    rightEqual (run "A.B (c)" "c = x") ([], [], "")
    rightEqual (run "A.B ((</>))" "x = a </> b")
        (["A.B"], [], "import A.B ((</>))\n")
    -- Add and remove operators
    rightEqual (run "A.B ((</>))" "x = a </> b")
        (["A.B"], [], "import A.B ((</>))\n")
    rightEqual (run "A.B ((</>))" "import A.B ((</>))\nx = a b")
        ([], ["A.B"], "")
    -- Removed unused.
    rightEqual (run "A.B (c)" "import A.B (c)\nx = x\n") ([], ["A.B"], "")
    rightEqual
        (run "A.B (c)"
            "import A.B (c)\nimport A.B (c, d)\nimport A.B (d)\nx = d\n")
        ([], [], "import A.B (d)\n")
    -- But not if it's a everything-import.
    rightEqual (run "A.B (c)" "import A.B\nx = x\n") ([], [], "import A.B\n")


-- * implementation

eResult :: FixImports.Result -> ([Types.ModuleName], [Types.ModuleName], String)
eResult r =
    ( Set.toList (FixImports.resultAdded r)
    , Set.toList (FixImports.resultRemoved r)
    , FixImports.resultImports r
    )

fixModule :: Index.Index -> [FilePath]
    -> Config.Config -> FilePath -> String -> Either String FixImports.Result
fixModule index files config modulePath source =
    case Unsafe.unsafePerformIO $
            FixImports.parse (Config._language config) modulePath source of
        Left err -> Left err
        Right (mod, cmts) ->
            fst $ Identity.runIdentity $ flip State.runStateT [] $
            FixImports.fixImports (pureFilesystem files) config index
                modulePath (FixImports.extract config mod cmts)

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
