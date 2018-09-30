{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
module Config_test where
import qualified Data.Map as Map
import qualified Data.Text as Text
import EL.Test.Global
import qualified EL.Test.Testing as Testing
import qualified Language.Haskell.Exts as Haskell

import qualified Config
import qualified FixImports
import qualified Types


test_parse = do
    let f = parseConfig
    equal (Config._order $
            f ["import-order-first: A", "import-order-last: B"]) $
        Config.Order
            { _importOrder = Config.Priority ["A"] ["B"]
            , _sortUnqualifiedLast = False
            }
    equal (Config._unqualified $ f ["unqualified: A.B.c D.E.(+)"]) $
        Map.fromList
            [ (Haskell.Ident () "c", "A.B")
            , (Haskell.Symbol () "+", "D.E")
            ]

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
    let f config imports = lines $ Config.formatGroups
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
