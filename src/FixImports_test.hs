module FixImports_test where
import Control.Monad (unless, void)
import qualified Data.Text as Text
import EL.Test.Global

import qualified Config


test_fixModule = do
    equal 1 1

    -- let f = FixModule.fixModule

mkConfig :: Text.Text -> Config.Config
mkConfig content
    | null errs = config
    | otherwise = error $ "parsing " <> show content  <> ": " <> unlines errs
    where (config, errs) = Config.parse content

-- mkConfig = Config.empty
--     { Config.showImports = Config.formatGroups $ Config.Priority
--         { high = []
--         , low = []
--         }
--     , Config.pickModule = Config.makePickModule defaultPriorities
--     }

-- importRange :: String -> Either String (Int, Int)
-- importRange modText = do
--     (mod, cmts) <- parse modText
--     return $ FixImports.importRange mod
--
-- cmtsInRange :: String -> Either String [Haskell.Comment]
-- cmtsInRange modText = do
--     (mod, cmts) <- parse modText
--     return $ FixImports.filterImportCmts (FixImports.importRange mod) cmts
--
-- parse :: String -> Either String (Types.Module, [Haskell.Comment])
-- parse text = case Haskell.parseFileContentsWithComments mode text of
--     Haskell.ParseFailed srcloc err ->
--         Left $ Haskell.prettyPrint srcloc ++ ": " ++ err
--     Haskell.ParseOk (mod, comments) -> Right (mod, comments)
--     where mode = Haskell.defaultParseMode
--
-- findModule :: [FilePath] -> FilePath -> String
--     -> IO (Maybe (Types.ModuleName, Bool))
-- findModule includes modulePath qual = do
--     index <- Index.loadIndex (Config.configIndex (Config.defaultConfig []))
--     FixImports.findModule includes index modulePath (Types.Qualification qual)

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
