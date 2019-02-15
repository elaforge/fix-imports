{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
module Config_test where
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified EL.Test.Testing as Testing
import qualified Language.Haskell.Exts as Haskell

import qualified Config
import qualified FixImports
import qualified Types

import           EL.Test.Global


test_parse = do
    let f = check . Config.parse . Text.unlines
        check (config, errs)
            | null errs = Right config
            | otherwise = Left $ Text.unlines errs
    equal (f []) $ Right Config.empty
    leftLike (f ["include: a", "include: b"]) "duplicate fields"
    equal (Config._order <$>
            f ["import-order-first: A", "import-order-last: B"]) $
        Right $ Config.Order
            { _importOrder = Config.Priority ["A"] ["B"]
            , _sortUnqualifiedLast = False
            }

test_parseUnqualified = do
    let f = Config._unqualified . parseConfig
    equal (f ["unqualified: A.B (c); D.E ((+))"]) $ Map.fromList
        [ (Haskell.Ident () "c", "A.B")
        , (Haskell.Symbol () "+", "D.E")
        ]
    equal (f ["unqualified: A.B(c, d)"]) $ Map.fromList
        [ (Haskell.Ident () "c", "A.B")
        , (Haskell.Ident () "d", "A.B")
        ]
    equal (f ["unqualified: A.B (c,(+))"]) $ Map.fromList
        [ (Haskell.Ident () "c", "A.B")
        , (Haskell.Symbol () "+", "A.B")
        ]

test_parseQualifyAs = do
    let f = Config._qualifyAs . parseConfig
    equal (f ["qualify-as: A.B as AB; E as F"]) $ Map.fromList
        [ ("AB", "A.B")
        , ("F", "E")
        ]
    stringsLike (snd $ Config.parse "qualify-as: gibble gabble")
        ["stanza should look like"]

test_pickModule = do
    let f config modulePath candidates = Config.pickModule
            (Config._modulePriority (parseConfig config))
            modulePath candidates
    equal (f [] "X.hs" []) Nothing
    let localAB = [(Nothing, "A.M"), (Nothing, "B.M")]
    equal (f [] "X.hs" localAB) $
        Just (Nothing, "A.M")
    equal (f ["prio-module-high: B.M"] "X.hs" localAB) $
        Just (Nothing, "B.M")
    -- Has to be an exact match.
    equal (f ["prio-module-high: B"] "X.hs" localAB) $
        Just (Nothing, "A.M")

    -- Local modules take precedence.
    equal (f [] "A/B.hs" [(Nothing, "B.M"), (Just "pkg", "B.M")]) $
        Just (Nothing, "B.M")
    equal (f [] "A/B/C.hs" [(Nothing, "A.B.M"), (Just "pkg", "B.M")]) $
        Just (Nothing, "A.B.M")
    -- Closer local modules precede further ones.
    equal (f [] "A/B/C.hs" [(Nothing, "A.B.M"), (Nothing, "A.M")]) $
        Just (Nothing, "A.B.M")
    -- Prefer fewer dots.
    equal (f [] "X.hs" [(Just "p1", "A.B.M"), (Just "p2", "Z.M")]) $
        Just (Just "p2", "Z.M")

test_formatGroups = do
    let f config imports = lines $ Config.formatGroups Config.defaultFormat
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

test_showImportLine = do
    let f = fmap (Config.showImportDecl style . Types.importDecl . head) . parse
        style = Config.defaultFormat { Config._leaveSpaceForQualified = True }
    -- pprint $ fmap (Types.importDecl . head) . parse $ "import A.B as C (x)"
    equal (f "import A.B as C (x)") $ Right "import           A.B as C (x)"

test_leaveSpaceForQualified = do
    let f columns leaveSpace =
            fmap (Config.showImportDecl (fmt columns leaveSpace) . head
                . map Types.importDecl)
            . parse
        fmt columns leaveSpace = Config.defaultFormat
            { Config._columns = columns
            , Config._leaveSpaceForQualified = leaveSpace
            }
    equal (f 80 False "import Foo.Bar (a, b, c)") $
        Right "import Foo.Bar (a, b, c)"
    equal (f 80 True "import Foo.Bar (a, b, c)") $
        Right "import           Foo.Bar (a, b, c)"

    equal (f 20 False "import Foo.Bar (a, b, c)") $
        Right "import Foo.Bar\n       (a, b, c)"
    equal (f 30 True "import Foo.Bar (a, b, c)") $
        Right "import           Foo.Bar\n       (a, b, c)"

    equal (f 30 True "import Foo.Bar (tweetle, beetle, paddle, battle)") $ Right
        "import           Foo.Bar\n\
        \       (tweetle, beetle,\n\
        \        paddle, battle)"

-- * util

importLine :: Types.ImportDecl -> Types.ImportLine
importLine decl = Types.ImportLine
    { importDecl = decl
    , importComments = []
    , importSource = Types.Local
    }

parse :: String -> Either String [Types.ImportLine]
parse source = case FixImports.parse [] "" source of
    Haskell.ParseFailed srcloc err ->
        Left $ Haskell.prettyPrint srcloc ++ ": " ++ err
    Haskell.ParseOk (mod, _cmts) ->
        Right $ map importLine $ FixImports.moduleImportDecls mod

parseConfig :: [Text.Text] -> Config.Config
parseConfig lines
    | null errs = config
    | otherwise = error $ "parsing " <> show lines  <> ": "
        <> Text.unpack (Text.unlines errs)
    where (config, errs) = Config.parse (Text.unlines lines)
