{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
module FixImports.Config_test where
import qualified Data.Map as Map
import qualified Data.Text as Text

import qualified FixImports.Config as Config
import qualified FixImports.FixImports as FixImports
import qualified FixImports.Types as Types

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
    rightEqual
        (Config._leaveSpaceForQualified . Config._format <$>
            f ["format:", "  leave-space-for-qualified"])
        True

test_parseUnqualified = do
    let f = Config._unqualified . parseConfig
    equal (f ["unqualified: A.B (c); D.E ((+))"]) $ Map.fromList
        [ (Types.Name "c", "A.B")
        , (Types.Operator "+", "D.E")
        ]
    equal (f ["unqualified: A.B(c, d)"]) $ Map.fromList
        [ (Types.Name "c", "A.B")
        , (Types.Name "d", "A.B")
        ]
    equal (f ["unqualified: A.B (c,(+))"]) $ Map.fromList
        [ (Types.Name "c", "A.B")
        , (Types.Operator "+", "A.B")
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


parseConfig :: [Text.Text] -> Config.Config
parseConfig lines
    | null errs = config
    | otherwise = error $ "parsing " <> show lines  <> ": "
        <> Text.unpack (Text.unlines errs)
    where (config, errs) = Config.parse (Text.unlines lines)
