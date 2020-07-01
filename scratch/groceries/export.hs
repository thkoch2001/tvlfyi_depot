module Main where

import Data.Function ((&))
import qualified Data.List as L

-- | Run this to export the grocery list.
main :: IO ()
main = do
  x <- readFile "./list.org"
  x & lines & filter (not . L.isPrefixOf "- 0x") & unlines & putStrLn
  pure ()
