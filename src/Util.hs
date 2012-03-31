module Util where
import Prelude hiding (head)
import qualified Control.Concurrent as Concurrent
import qualified Control.Concurrent.MVar as MVar
import qualified Control.Exception as Exception
import Control.Monad

import qualified Data.Function as Function
import qualified Data.List as List
import qualified Data.List.Split as Split
import qualified Data.Text as T
import qualified Data.Text.IO as Text.IO

import qualified System.Directory as Directory
import qualified System.Exit as Exit
import System.FilePath ((</>))
import qualified System.IO as IO
import qualified System.IO.Error as IO.Error
import qualified System.Process as Process


-- * list

-- | Concat a list with 'sep' in between.
join :: [a] -> [[a]] -> [a]
join = List.intercalate

-- | Split 'xs' on 'sep', dropping 'sep' from the result.
split :: (Eq a) => [a] -> [a] -> [[a]]
split = Split.splitOn

head :: [a] -> Maybe a
head [] = Nothing
head (x:_) = Just x

sortOn :: (Ord k) => (a -> k) -> [a] -> [a]
sortOn key = List.sortBy (compare `Function.on` key)

groupOn :: (Eq k) => (a -> k) -> [a] -> [[a]]
groupOn key = List.groupBy ((==) `Function.on` key)

-- * control

ifM :: (Monad m) => m Bool -> m a -> m a -> m a
ifM cond consequent alternative = do
    b <- cond
    if b then consequent else alternative

partitionM :: (Monad m) => (a -> m Bool) -> [a] -> m ([a], [a])
partitionM f = go [] []
    where
    go ts fs [] = return (ts, fs)
    go ts fs (x:xs) = ifM (f x) (go (x:ts) fs xs) (go ts (x:fs) xs)

anyM :: (a -> IO Bool) -> [a] -> IO Bool
anyM _ [] = return False
anyM f (x:xs) = ifM (f x) (return True) (anyM f xs)

-- | If @op@ raised ENOENT, return Nothing.
catchENOENT :: IO a -> IO (Maybe a)
catchENOENT op = Exception.handleJust (guard . IO.Error.isDoesNotExistError)
    (const (return Nothing)) (fmap Just op)

-- * file

listDir :: FilePath -> IO [FilePath]
listDir dir = fmap (map add . filter (not . (`elem` [".", ".."])))
        (Directory.getDirectoryContents dir)
    where add = if dir == "." then id else (dir </>)

-- | Similar to System.Process.readProcessWithExitCode but return Text instead
-- of String.
readProcessWithExitCode :: FilePath -> [String]
    -> IO (Exit.ExitCode, T.Text, T.Text)
readProcessWithExitCode cmd args = do
    (_, Just outh, Just errh, pid) <-
        Process.createProcess (Process.proc cmd args)
            { Process.std_out = Process.CreatePipe
            , Process.std_err = Process.CreatePipe
            }
    outMVar <- MVar.newEmptyMVar
    errMVar <- MVar.newEmptyMVar
    void $ Concurrent.forkIO $
        MVar.putMVar outMVar =<< Text.IO.hGetContents outh
    void $ Concurrent.forkIO $
        MVar.putMVar errMVar =<< Text.IO.hGetContents errh
    out <- MVar.takeMVar outMVar
    err <- MVar.takeMVar errMVar
    IO.hClose outh
    IO.hClose errh
    ex <- Process.waitForProcess pid
    return (ex, out, err)

