module Main where

import qualified Data.List as L

(|>) :: a -> (a -> b) -> b
x |> f = f x

-- | Ignore items with zero quantity (i.e. "0x") and comments (i.e. "#")
isUndesirableOutput :: String -> Bool
isUndesirableOutput x =
  (L.isPrefixOf "- 0x" x) || (L.isPrefixOf "#" x)

-- | Run this to export the grocery list.
main :: IO ()
main = do
  content <- readFile "./list.org"
  content
    |> lines
    |> filter (not . isUndesirableOutput)
    |> unlines
    |> putStrLn
  pure ()
