module F
  ( join
  , split
  ) where

--------------------------------------------------------------------------------
-- Dependencies
--------------------------------------------------------------------------------

import Data.List (span)
import System.FilePath (FilePath, pathSeparator)
import System.FilePath.Posix (FilePath)
import qualified System.FilePath.Posix as F

-- TODO: Move this to a misc.hs, prelude.hs, operators.hs; somewhere.
(|>) :: a -> (a -> b) -> b
(|>) a f = f a
infixl 1 |>

-- TODO: Move this to a test_utils.hs or elsewhere.
simpleAssert :: (Eq a) => a -> a -> ()
simpleAssert x y =
  if x == y then
    ()
  else
    error "Assertion error"

--------------------------------------------------------------------------------
-- Library
--------------------------------------------------------------------------------

join :: [FilePath] -> FilePath
join = F.joinPath

-- | Split path and return  list containing parts.
split :: FilePath -> [String]
split = splitJoin . span (/= pathSeparator)
  where
    splitJoin :: (String, String) -> [String]
    splitJoin ([], []) = []
    splitJoin (a, []) = [a]
    splitJoin (a, [_]) = [a]
    splitJoin (a, _:b) = a : split b

--------------------------------------------------------------------------------
-- Tests
--------------------------------------------------------------------------------

expected :: [([FilePath], FilePath)]
expected = [ (["path"], "path")
           , (["/path"], "/path")
           , (["path", "to", "file"], "path/to/file")
           , (["/path", "to", "file"], "/path/to/file")
           , (["/"], "/")
           ]

runTests :: [()]
runTests =
  fmap (\(input, expected) -> simpleAssert (join input) expected) expected

main :: IO ()
main = do
  print runTests
  pure ()
