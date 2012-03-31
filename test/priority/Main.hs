-- | Should import Data.Text and not GHC.Something.Something
-- Should chose local Char over M.Char and Data.Char.
-- Should choose System.IO over System.Posix.IO
module Main where

x = Text.pack Char.toUpper IO.putStrLn
