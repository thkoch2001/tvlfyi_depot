--------------------------------------------------------------------------------
module Keyboard where
--------------------------------------------------------------------------------
import Utils
import qualified Data.List as List
--------------------------------------------------------------------------------

newtype Keyboard = Keyboard [[Char]]
  deriving (Eq)

instance Show Keyboard where
  show (Keyboard xxs) =
    xxs |> fmap printRow |> List.intercalate "\n"
    where
      printRow :: [Char] -> String
      printRow xs =
        xs |> fmap (\x -> '[':x:']':"") |> List.intercalate ""

qwerty :: Keyboard
qwerty = Keyboard [ ['1','2','3','4','5','6','7','8','9','0']
                  , ['Q','W','E','R','T','Y','U','I','O','P']
                  , ['A','S','D','F','G','H','J','K','L',';']
                  , ['Z','X','C','V','B','N','M',',','.','/']
                  ]
