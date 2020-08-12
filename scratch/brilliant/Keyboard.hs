{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
--------------------------------------------------------------------------------
module Keyboard where
--------------------------------------------------------------------------------
import Utils
import Data.Coerce
import Data.Hashable (Hashable)
import GHC.Generics (Generic)

import qualified Data.List as List
import qualified Data.HashMap.Strict as HM
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

data Coord = Coord
  { row :: Int
  , col :: Int
  } deriving (Eq, Show, Generic)

instance Hashable Coord

-- | List of characters to their QWERTY coordinatees.
coords :: [(Char, Coord)]
coords =
  qwerty
  |> coerce
  |> fmap (zip [0..])
  |> zip [0..]
  |> fmap (\(row, xs) -> xs |> fmap (\(col, char) -> (char, Coord row col)))
  |> mconcat

-- | Mapping of characters to their coordinates on a QWERTY keyboard with the
-- top-left corner as 0,0.
charToCoord :: HM.HashMap Char Coord
charToCoord = HM.fromList coords

coordToChar :: Keyboard -> Coord -> Maybe Char
coordToChar (Keyboard xxs) Coord{..} =
  Just $ xxs !! row !! col

qwerty :: Keyboard
qwerty = Keyboard [ ['1','2','3','4','5','6','7','8','9','0']
                  , ['Q','W','E','R','T','Y','U','I','O','P']
                  , ['A','S','D','F','G','H','J','K','L',';']
                  , ['Z','X','C','V','B','N','M',',','.','/']
                  ]
