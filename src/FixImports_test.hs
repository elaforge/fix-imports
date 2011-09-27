-- | Grody hand-testing because I'm too lazy to libraryize the test framework
-- just for this.
module FixImports_test where
import qualified Language.Haskell.Exts.Annotated as Haskell

import qualified FixImports
import qualified Types


importRange modText = do
    (mod, cmts) <- parse modText
    return $ FixImports.importRange mod

cmtsInRange modText = do
    (mod, cmts) <- parse modText
    return $ FixImports.filterImportCmts (FixImports.importRange mod) cmts

parse :: String -> Either String (Types.Module, [Haskell.Comment])
parse text = case Haskell.parseFileContentsWithComments mode text of
    Haskell.ParseFailed srcloc err ->
        Left $ Haskell.prettyPrint srcloc ++ ": " ++ err
    Haskell.ParseOk (mod, comments) -> Right (mod, comments)
    where mode = Haskell.defaultParseMode

-- TODO quasi-quoting has a nicer way for multi-line strings, right?
tmod0 = "-- cmt1\n\
\-- cmt2\n\
\import Data.List\n\
\x = 42\n"

tmod1 = "module Foo\nwhere\n" ++ tmod0
tmod2 = "module Foo\nwhere\nx = 42\n"

tmod = "{-# LANGUAGE SomeExtension #-} -- comment on LANGUAGE\n\
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
