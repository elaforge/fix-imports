-- | Main file for FixImports that uses the default formatting.  It reads
-- a config file from the current directory.
--
-- More documentation in "FixImports".
{-# LANGUAGE DisambiguateRecordFields #-}
module Main where
import qualified Data.Text.IO as Text.IO
import qualified System.IO as IO

import qualified Config
import qualified FixImports
import qualified Util


main :: IO ()
main = do
    (config, errors) <- fmap (maybe (Config.empty, []) Config.parse) $
        Util.catchENOENT $ Text.IO.readFile ".fix-imports"
    mapM_ (IO.hPutStrLn IO.stderr) errors
    FixImports.runMain config
