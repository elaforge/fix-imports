module Util where
import Prelude hiding (head)
import qualified Control.Concurrent as Concurrent
import qualified Control.Concurrent.MVar as MVar
import qualified Control.Exception as Exception
import Control.Monad

import qualified Data.Char as Char
import qualified Data.Function as Function
import qualified Data.IntSet as IntSet
import qualified Data.List as List
import qualified Data.List.Split as Split
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.IO as Text.IO

import qualified System.Directory as Directory
import qualified System.Exit as Exit
import System.FilePath ((</>))
import qualified System.IO as IO
import qualified System.IO.Error as IO.Error
import qualified System.Process as Process


-- | Copy paste from FastTags.Tag.haskellOpChar.
--
-- From the haskell report:
-- > varsym   →   ( symbol⟨:⟩ {symbol} )⟨reservedop | dashes⟩
-- > symbol   →   ascSymbol | uniSymbol⟨special | _ | " | '⟩
-- > uniSymbol    →   any Unicode symbol or punctuation
haskellOpChar :: Char -> Bool
haskellOpChar c =
    IntSet.member (Char.ord c) opChars
    || (IntSet.notMember (Char.ord c) exceptions
        && isSymbolCharacterCategory (Char.generalCategory c))
    where
    opChars = IntSet.fromList $ map Char.ord "-!#$%&*+./<=>?@^|~:\\"
    exceptions = IntSet.fromList $ map Char.ord "_\"'"

isSymbolCharacterCategory :: Char.GeneralCategory -> Bool
isSymbolCharacterCategory cat = Set.member cat symbolCategories
    where
    symbolCategories :: Set.Set Char.GeneralCategory
    symbolCategories = Set.fromList
        [ Char.ConnectorPunctuation
        , Char.DashPunctuation
        , Char.OtherPunctuation
        , Char.MathSymbol
        , Char.CurrencySymbol
        , Char.ModifierSymbol
        , Char.OtherSymbol
        ]

-- * list

-- | List initial and final element, if any.
unsnoc :: [a] -> Maybe ([a], a)
unsnoc [] = Nothing
unsnoc (x:xs) = Just $ go x xs
    where
    go x [] = ([], x)
    go x (x':xs) = let (pre, post) = go x' xs in (x:pre, post)

-- | Concat a list with 'sep' in between.
join :: [a] -> [[a]] -> [a]
join = List.intercalate

-- | Split 'xs' on 'sep', dropping 'sep' from the result.
split :: (Eq a) => [a] -> [a] -> [[a]]
split = Split.splitOn

-- | Split where the function matches.
splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith f xs = map reverse (go f xs [])
    where
    go _ [] collect = [collect]
    go f (x:xs) collect
        | f x = collect : go f xs [x]
        | otherwise = go f xs (x:collect)

head :: [a] -> Maybe a
head [] = Nothing
head (x:_) = Just x

sortOn :: (Ord k) => (a -> k) -> [a] -> [a]
sortOn key = List.sortBy (compare `Function.on` key)

groupOn :: (Eq k) => (a -> k) -> [a] -> [[a]]
groupOn key = List.groupBy ((==) `Function.on` key)

minimumOn :: Ord k => (a -> k) -> [a] -> Maybe a
minimumOn _ [] = Nothing
minimumOn key xs = Just (List.foldl1' f xs)
    where f low x = if key x < key low then x else low

-- | Like 'List.partition', but partition by two functions consecutively.
partition2 :: (a -> Bool) -> (a -> Bool) -> [a] -> ([a], [a], [a])
partition2 f1 f2 xs = (as, bs, xs3)
    where
    (as, xs2) = List.partition f1 xs
    (bs, xs3) = List.partition f2 xs2

zipPrev :: [a] -> [(a, a)]
zipPrev xs = zip xs (drop 1 xs)

-- | Modify a list at the first place the predicate matches.
modifyAt :: (a -> Bool) -> (a -> a) -> [a] -> Maybe [a]
modifyAt match modify = go
    where
    go [] = Nothing
    go (x:xs)
        | match x = Just (modify x : xs)
        | otherwise = (x:) <$> go xs

multimap :: Ord k => [(k, a)] -> Map.Map k [a]
multimap = Map.fromAscList . map (\gs -> (fst (List.head gs), map snd gs))
    . groupOn fst . sortOn fst

partitionOn :: (a -> Maybe b) -> [a] -> ([b], [a])
partitionOn f = go
    where
    go [] = ([], [])
    go (x:xs) = case f x of
        Just b -> (b:bs, as)
        Nothing -> (bs, x:as)
        where (bs, as) = go xs

-- * control

ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM cond consequent alternative = do
    b <- cond
    if b then consequent else alternative

partitionM :: (Monad m) => (a -> m Bool) -> [a] -> m ([a], [a])
partitionM f = go [] []
    where
    go ts fs [] = return (ts, fs)
    go ts fs (x:xs) = ifM (f x) (go (x:ts) fs xs) (go ts (x:fs) xs)

anyM :: Monad m => (a -> m Bool) -> [a] -> m Bool
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

