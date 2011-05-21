import qualified FixImports as FixImports
import qualified Config

main :: IO ()
main = FixImports.runMain (Config.defaultConfig priorities)

priorities :: [String]
priorities =
    [ "Util", "Midi", "Ui", "Cmd", "Derive", "Perform"
    , "Instrument", "Local", "LogView"
    , "App"]
