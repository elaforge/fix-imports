{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
module FixImports.Parse_test where
import qualified Data.Maybe as Maybe
import           Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import qualified System.IO.Unsafe as Unsafe

import qualified GHC.Hs as Hs
import qualified GHC.Types.SrcLoc as SrcLoc

import qualified FixImports.Parse as Parse
import qualified FixImports.Types as Types

import           EL.Test.Global


test_importRange = do
    let f = fmap Parse.importRange . parse
    rightEqual (f "") (0, 0)
    rightEqual (f "module M where\n") (1, 1)
    rightEqual (f "-- hi\nmodule M where\n") (2, 2)
    rightEqual (f
        "module M (\n\
        \    x, y\n\
        \) where\n\
        \import A\n\
        \import B\n\
        \f = 42\n")
        (3, 5)
    rightEqual (f
        "module M (\n\
        \    x, y\n\
        \) where\n\
        \f = 42\n")
        (3, 3)
    rightEqual (f
        "module M where\n\
        \f = 42\n")
        (1, 1)

test_extractImports = do
    let f = fmap (head . map strip . Parse.extractImports) . parse
        strip imp = imp { Types._importSpan = Types.noSpan }
    rightEqual (f "import \"pkg\" A ()") $ (Types.makeImport "A")
        { Types._importPkgQualifier = Just "pkg"
        , Types._importEntities = Just []
        }
    rightEqual (f "import {-# SOURCE #-} A") $ (Types.makeImport "A")
        { Types._importIsBoot = True }
    rightEqual (f "import A as B hiding (a, b)") $ (Types.makeImport "A")
        { Types._importAs = Just "B"
        , Types._importHiding = True
        , Types._importEntities = Just
            [ Right $ Types.Entity Nothing (Types.Name "a") Nothing
            , Right $ Types.Entity Nothing (Types.Name "b") Nothing
            ]
        }

test_extensions = do
    let f exts = Parse.parse (map (expectRight . parseExtension) exts) "M.hs"
    let extract = fmap (fmap (extractEntities . fst))
    apply1 leftLike (extract $ f [] "import X (pattern A)") "parse error"
    apply1 rightEqual (extract $ f ["PatternSynonyms"] "import X (pattern A)")
        [Right ("pattern", Types.Name "A", "")]
    apply1 rightEqual
        (extract $ f []
            "{-# LANGUAGE PatternSynonyms #-}\nimport X (pattern A)")
        [Right ("pattern", Types.Name "A", "")]

parseExtension :: String -> Either String Types.Extension
parseExtension w = maybe (Left w) Right $ Types.parseExtension w

test_comments = do
    let f = fmap (fmap (map extract . snd)) . Parse.parse [] "M.hs"
        extract (Parse.Comment (Types.SrcSpan x1 y1 x2 y2) cmt) =
            ((x1, y1, x2, y2), cmt)
    apply1 rightEqual (f "module M where\n") []
    apply1 rightEqual (f "-- above\nimport X -- right\n")
        [((0, 1, 0, 9), "-- above"), ((1, 10, 1, 18), "-- right")]
    apply1 rightEqual (f "-- | above\nimport X {- right -}\n")
        [((0, 1, 0, 11), "-- | above"), ((1, 10, 1, 21), "{- right -}")]
    apply1 rightEqual (f "-- *** section\nimport X {- ^ right -}\n")
        [((0, 1, 0, 15), "-- *** section"), ((1, 10, 1, 23), "{- ^ right -}")]

test_extractEntity = do
    let f = fmap extractEntities . parse
        extractEntities = map (fmap extractEntity) . fromMaybe []
            . Types._importEntities . head . Parse.extractImports
        extractEntity (Types.Entity qual name list) =
            (fromMaybe "" qual, name, fromMaybe "" list)
    let n = Types.Name
    rightEqual (f "import X") []
    rightEqual (f "import X (a, b)")
        [Right ("", n "a", ""), Right ("", n "b", "")]
    rightEqual (f "import X (type A)") [Right ("type", n "A", "")]

    rightEqual (f "import X (A)") [Right ("", n "A", "")]
    rightEqual (f "import X (A(..))") [Right ("", n "A", "(..)")]
    rightEqual (f "import X (A())") [Right ("", n "A", "()")]
    rightEqual (f "import X (A(b, c))") [Right ("", n "A", "(b, c)")]
    rightEqual (f "import X (A(B))") [Right ("", n "A", "(B)")]
    rightEqual (f "import X ((*))") [Right ("", Types.Operator "*", "")]


extractEntities = map (fmap extractEntity) . fromMaybe []
    . Types._importEntities . head . Parse.extractImports
    where
    extractEntity (Types.Entity qual name list) =
        (fromMaybe "" qual, name, fromMaybe "" list)

test_qualifications = do
    let f = fmap (Set.toList . Parse.qualifications) . parse
    rightEqual (f "f = x") []
    rightEqual (f "f = A.x") ["A"]
    rightEqual (f "f = A.B.x D.y") ["A.B", "D"]
    rightEqual (f "f :: A.X -> B.Y") ["A", "B"]
    rightEqual (f "f = x A.</> y") ["A"]
    rightEqual (f "f = (A.</>) x y") ["A"]
    rightEqual (f "f = g @A.B @C.D") ["A", "C"]

test_unqualifiedValues = do
    let f = fmap (map unname . Set.toList . Parse.unqualifiedValues) . parse
        unname (Types.Name s) = s
        unname (Types.Operator s) = "(" <> s <> ")"
    -- Don't pick up imports and exports.
    rightEqual (f "module A (b) where\nimport C (d)\n") []
    -- Don't pick up function lhs.
    rightEqual (f "f x = 10") []
    rightEqual (f "f x = y") ["y"]
    rightEqual (f "x = y") ["y"]
    -- TODO I'd rather just "pat", but I'd need to dig deeper into
    -- the pattern AST which is complicated.
    rightEqual (f "f | x <- pat = 10") ["pat", "x"]
    rightEqual (f "f = 10 * 20") ["(*)"]
    -- instance declarations
    rightEqual (f "instance A B where f = x") ["x"]

test_unqualifiedTypes = do
    let f = fmap (map unname . Set.toList . Parse.unqualifiedTypes) . parse
        unname (Types.Name s) = s
        unname (Types.Operator s) = "(" <> s <> ")"
    -- Also look in type signatures.
    rightEqual (f "f :: A -> B C") ["A", "B", "C"]
    rightEqual (f "instance A b where f = x") ["A"]
    rightEqual (f "instance A b where f = x") ["A"]
    -- But not type declarations.
    rightEqual (f "data A = B c | D e deriving (Z)") ["Z"]
    rightEqual (f "class A where f :: A") ["A"]

apply1 :: Monad m => (a -> b -> m c) -> m a -> b -> m c
apply1 f ma b = do
    a <- ma
    f a b

parse :: String -> Either String Parse.Module
parse = Unsafe.unsafePerformIO . fmap (fmap fst) . Parse.parse [] "M.hs"
