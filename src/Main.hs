-- | Main file for FixImports that uses the default formatting.  Since import
-- priority is per-project, it reads it from a file in the current directory.
--
-- More documentation in "FixImports".
module Main where
import qualified System.Directory as Directory

import qualified Config
import qualified FixImports
import qualified Util


main :: IO ()
main = do
    priorities <- fmap words (readEmpty ".fix-imports-priority")
    FixImports.runMain (Config.defaultConfig priorities)

readEmpty :: FilePath -> IO String
readEmpty fn = Util.ifM (Directory.doesFileExist fn) (readFile fn) (return "")
