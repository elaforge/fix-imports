-- | Main file for FixImports that uses the default formatting.  It reads
-- a config file from the current directory.
--
-- More documentation in "FixImports".
module Main where
import Control.Monad
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO

import qualified System.IO as IO

import qualified Config
import qualified FixImports
import qualified Index
import qualified Types
import qualified Util


main :: IO ()
main = do
    (config, errors) <- fmap (maybe (Config.empty, []) parse) $
        Util.catchENOENT $ Text.IO.readFile ".fix-imports"
    mapM_ (IO.hPutStrLn IO.stderr) errors
    FixImports.runMain config

parse :: Text.Text -> (Config.Config, [String])
parse text = (config, errors)
    where
    commas = List.intercalate ", "
    errors =
        [ ".fix-imports has unrecognized fields: "
            ++ commas unknownFields | not (null unknownFields) ]
        ++ [ ".fix-imports has unknown language extensions: "
            ++ commas unknownLanguage | not (null unknownLanguage) ]
    config = Config.empty
        { Config.includes = get "include"
        , Config.language = language
        , Config.showImports = Config.formatGroups order
        , Config.pickModule = Config.makePickModule prios
        }
    (unknownLanguage, language) = Config.parseLanguage (get "language")
    unknownFields = Map.keys fields List.\\ valid
    valid =
        [ "include", "import-order"
        , "prio-package-high", "prio-package-low"
        , "prio-module-high", "prio-module-low"
        , "language"
        ]
    order = Config.ImportOrder (getModules "import-order")
    prios = Config.Priorities (get "prio-package-high", get "prio-package-low")
        (getModules "prio-module-high", getModules "prio-module-low")
    fields = Map.fromList [(Text.unpack section, map Text.unpack words)
        | (section, words) <- Index.parseSections text]
    getModules = map Types.ModuleName . get
    get k = Map.findWithDefault [] k fields
