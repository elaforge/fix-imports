module Util where
import Prelude hiding (head)
import Control.Monad
import qualified Control.Exception as Exception
import qualified Data.Function as Function
import qualified Data.List as List
import qualified Data.List.Split as Split

import qualified System.Directory as Directory
import System.FilePath ((</>))
import qualified System.IO.Error as IO.Error


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
