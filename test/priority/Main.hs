-- | Should import Data.Text and not GHC.Something.Something
-- Should chose local Char over M.Char and Data.Char.
module Main where

x = Text.pack Char.toUpper
