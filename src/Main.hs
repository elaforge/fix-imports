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
    let deflt = (Config.ImportOrder [], Config.defaultPriorities, [])
    (order, prios, warns) <- fmap (maybe deflt parse) $
        Util.catchENOENT $ Text.IO.readFile ".fix-imports"
    unless (null warns) $
        IO.hPutStrLn IO.stderr $
            "warnings unrecognized fields in .fix-imports: "
            ++ List.intercalate ", " warns
    FixImports.runMain (Config.config order prios)

parse :: Text.Text -> (Config.ImportOrder, Config.Priorities, [String])
parse text = (order, prios, extra)
    where
    extra = Map.keys config List.\\ valid
    valid = ["import-order", "prio-package-high", "prio-package-low",
        "prio-module-high", "prio-module-low"]
    order = Config.ImportOrder (getModules "import-order")
    prios = Config.Priorities (get "prio-package-high", get "prio-package-low")
        (getModules "prio-module-high", getModules "prio-module-low")
    config = Map.fromList [(Text.unpack section, map Text.unpack words)
        | (section, words) <- Index.parseSections text]
    getModules = map Types.ModuleName . get
    get k = Map.findWithDefault [] k config
