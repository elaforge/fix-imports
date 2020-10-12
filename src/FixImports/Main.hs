-- | Main file for FixImports that uses the default formatting.  It reads
-- a config file from the current directory.
--
-- More documentation in "FixImports".
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module FixImports.Main where
import qualified Control.Exception as Exception
import Control.Monad (when)
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import qualified Data.Version as Version

import qualified System.Console.GetOpt as GetOpt
import qualified System.Environment as Environment
import qualified System.Exit as Exit
import qualified System.IO as IO

import qualified FixImports.Config as Config
import qualified FixImports.FixImports as FixImports
import qualified Paths_fix_imports
import qualified FixImports.Types as Types
import qualified FixImports.Util as Util


main :: IO ()
main = do
    -- I need the module path to search for modules relative to it first.  I
    -- could figure it out from the parsed module name, but a main module may
    -- not have a name.
    (modulePath, flags) <- parseArgs =<< Environment.getArgs
    let configFile = if null fns then ".fix-imports" else last fns
            where fns = [fn | Config fn <- flags]
    (config, errors) <- readConfig configFile
    if null errors
        then mainConfig config flags modulePath
        else do
            Text.IO.hPutStrLn IO.stderr $ Text.unlines errors
            Exit.exitFailure

readConfig :: FilePath -> IO (Config.Config, [Text.Text])
readConfig = fmap (maybe (Config.empty, []) Config.parse)
    . Util.catchENOENT . Text.IO.readFile

mainConfig :: Config.Config -> [Flag] -> FilePath -> IO ()
mainConfig config flags modulePath = do
    let (verbose, debug, includes) = extractFlags flags
    source <- IO.getContents
    config <- return $ config
        { Config._includes = includes ++ Config._includes config
        , Config._debug = debug
        }
    (result, logs) <- FixImports.fixModule config modulePath source
        `Exception.catch` \(exc :: Exception.SomeException) ->
            return (Left $ "exception: " ++ show exc, [])
    case result of
        Left err -> do
            if Edit `elem` flags then putStrLn "0,0" else putStr source
            when debug $ mapM_ (Text.IO.hPutStrLn IO.stderr) logs
            IO.hPutStrLn IO.stderr $ "error: " ++ err
            Exit.exitFailure
        Right (FixImports.Result range imports added removed metrics) -> do
            if Edit `elem` flags
                then do
                    putStrLn $ show (fst range) <> "," <> show (snd range)
                    putStr imports
                else putStr $ FixImports.substituteImports imports range source
            let names = Util.join ", " . map Types.moduleName . Set.toList
                (addedMsg, removedMsg) = (names added, names removed)
            mDone <- FixImports.metric metrics "done"
            when debug $ mapM_ (Text.IO.hPutStrLn IO.stderr) logs
            Config.debug config $ Text.stripEnd $
                FixImports.showMetrics (mDone : metrics)
            when (verbose && not (null addedMsg) || not (null removedMsg)) $
                IO.hPutStrLn IO.stderr $ Util.join "; " $ filter (not . null)
                    [ if null addedMsg then "" else "added: " ++ addedMsg
                    , if null removedMsg then "" else "removed: " ++ removedMsg
                    ]
            Exit.exitSuccess

data Flag = Config FilePath | Debug | Edit | Include String | Verbose
    deriving (Eq, Show)

options :: [GetOpt.OptDescr Flag]
options =
    [ GetOpt.Option ['c'] ["config"] (GetOpt.ReqArg Config "path")
        "path to config file, defaults to .fix-imports"
    , GetOpt.Option [] ["edit"] (GetOpt.NoArg Edit)
        "print delete range and new import block, rather than the whole file"
    , GetOpt.Option [] ["debug"] (GetOpt.NoArg Debug)
        "print debugging info on stderr"
    , GetOpt.Option ['i'] [] (GetOpt.ReqArg Include "path")
        "add to module include path"
    , GetOpt.Option ['v'] [] (GetOpt.NoArg Verbose)
        "print added and removed modules on stderr"
    ]

parseArgs :: [String] -> IO (String, [Flag])
parseArgs args = case GetOpt.getOpt GetOpt.Permute options args of
    (flags, [modulePath], []) -> return (modulePath, flags)
    (_, [], errs) -> usage $ concat errs
    _ -> usage "too many args"

extractFlags :: [Flag] -> (Bool, Bool, [FilePath])
extractFlags flags =
    ( Verbose `elem` flags
    , Debug `elem` flags
    , "." : [p | Include p <- flags]
    )
    -- Includes always have the current directory first.

usage :: String -> IO a
usage msg = do
    name <- Environment.getProgName
    IO.hPutStr IO.stderr $
        GetOpt.usageInfo (msg ++ "\n" ++ name ++ " Module.hs <Module.hs")
            options
    IO.hPutStrLn IO.stderr $
        "version: " ++ Version.showVersion Paths_fix_imports.version
    Exit.exitFailure
