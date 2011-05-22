module Util where
import qualified Data.Function as Function
import qualified Data.List as List
import qualified Data.List.Split as Split
import qualified System.Directory as Directory
import System.FilePath ( (</>) )


-- * list

-- | Concat a list with 'sep' in between.
join :: [a] -> [[a]] -> [a]
join sep = concat . List.intersperse sep

-- | Split 'xs' on 'sep', dropping 'sep' from the result.
split :: (Eq a) => [a] -> [a] -> [[a]]
split = Split.splitOn

-- * control

ifM :: (Monad m) => m Bool -> m a -> m a -> m a
ifM cond consequent alternative = do
    b <- cond
    if b then consequent else alternative

mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe f xs = [b | Just b <- map f xs]

groupOn :: (Eq k) => (a -> k) -> [a] -> [[a]]
groupOn key = List.groupBy ((==) `Function.on` key)

sortOn :: (Ord k) => (a -> k) -> [a] -> [a]
sortOn key = List.sortBy (compare `Function.on` key)

-- | Keep running the action until it returns a Just.
untilJust :: (a -> IO (Maybe b)) -> [a] -> IO (Maybe b)
untilJust _ [] = return Nothing
untilJust f (x:xs) = maybe (untilJust f xs) (return . Just) =<< f x

-- * file

listDir :: FilePath -> IO [FilePath]
listDir dir = fmap (map add . filter (not . (`elem` [".", ".."])))
        (Directory.getDirectoryContents dir)
    where add = if dir == "." then id else (dir </>)
