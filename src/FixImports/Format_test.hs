{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
module FixImports.Format_test where
import qualified Data.Text as Text
import qualified System.IO.Unsafe as Unsafe

import qualified FixImports.Config as Config
import qualified FixImports.Format as Format
import qualified FixImports.Parse as Parse
import qualified FixImports.Types as Types

import qualified EL.Test.Testing as Testing
import           EL.Test.Global


test_formatGroups = do
    let f config imports = lines $ Format.formatGroups Config.defaultFormat
            (Config._order (parseConfig config))
            (Testing.expectRight (parse (unlines imports)))
    equal (f [] []) []
    -- Unqualified import-all goes last.
    equal (f ["sort-unqualified-last: t"]
            [ "import Z", "import A"
            , "import qualified C", "import qualified B"
            , "import C (a)"
            ])
        [ "import qualified B"
        , "import qualified C"
        , "import C (a)"
        , ""
        , "import A"
        , "import Z"
        ]

    equal (f [] ["import qualified Z", "import qualified A"])
        [ "import qualified A"
        , "import qualified Z"
        ]
    equal (f ["import-order-first: Z"]
            ["import qualified Z", "import qualified A"])
        [ "import qualified Z"
        , "import qualified A"
        ]
    equal (f ["import-order-last: A"]
            ["import qualified Z", "import qualified A"])
        [ "import qualified Z"
        , "import qualified A"
        ]

    -- Exact match.
    equal (f ["import-order-first: Z"]
            ["import qualified Z.A", "import qualified A"])
        [ "import qualified A"
        , "import qualified Z.A"
        ]
    -- Unless it's a prefix match.
    equal (f ["import-order-first: Z."]
            ["import qualified Z.A", "import qualified A"])
        [ "import qualified Z.A"
        , "import qualified A"
        ]

test_showImport = do
    let f = fmap (Format.showImport style . Types.importDecl . head) . parse
        style = Config.defaultFormat { Config._leaveSpaceForQualified = True }
    equal (f "import A.B as C (x)") $ Right "import           A.B as C (x)"

test_leaveSpaceForQualified = do
    let f columns leaveSpace =
            fmap (Format.showImport (fmt columns leaveSpace) . head
                . map Types.importDecl)
            . parse
        fmt columns leaveSpace = Config.defaultFormat
            { Config._columns = columns
            , Config._leaveSpaceForQualified = leaveSpace
            }
    rightEqual (f 80 False "import Foo.Bar (a, b, c)")
        "import Foo.Bar (a, b, c)"
    rightEqual (f 80 True "import Foo.Bar (a, b, c)")
        "import           Foo.Bar (a, b, c)"

    rightEqual (f 20 False "import Foo.Bar (a, b, c)")
        "import Foo.Bar\n\
        \    (a, b, c)"
    rightEqual (f 30 True "import Foo.Bar (a, b, c)")
        "import           Foo.Bar\n\
        \    (a, b, c)"

    rightEqual (f 30 True "import Foo.Bar (tweetle, beetle, paddle, battle)")
        "import           Foo.Bar\n\
        \    (tweetle, beetle, paddle,\n\
        \     battle)"


-- * util

parse :: String -> Either String [Types.ImportLine]
parse = Unsafe.unsafePerformIO
    . fmap (fmap (map importLine . Parse.extractImports . fst))
    . Parse.parse [] "M.hs"

importLine :: Types.Import -> Types.ImportLine
importLine imp = Types.ImportLine
    { importDecl = imp
    , importComments = []
    , importSource = Types.Local
    }

parseConfig :: [Text.Text] -> Config.Config
parseConfig lines
    | null errs = config
    | otherwise = error $ "parsing " <> show lines  <> ": "
        <> Text.unpack (Text.unlines errs)
    where (config, errs) = Config.parse (Text.unlines lines)
