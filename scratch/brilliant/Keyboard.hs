{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
--------------------------------------------------------------------------------
module Keyboard where
--------------------------------------------------------------------------------
import Utils
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
coords = [ ('0', Coord { row = 0, col = 0 })
         , ('1', Coord { row = 0, col = 1 })
         , ('2', Coord { row = 0, col = 2 })
         , ('3', Coord { row = 0, col = 3 })
         , ('4', Coord { row = 0, col = 4 })
         , ('5', Coord { row = 0, col = 5 })
         , ('6', Coord { row = 0, col = 6 })
         , ('7', Coord { row = 0, col = 7 })
         , ('8', Coord { row = 0, col = 8 })
         , ('9', Coord { row = 0, col = 9 })
         -- second row
         , ('Q', Coord { row = 1, col = 0 })
         , ('W', Coord { row = 1, col = 1 })
         , ('E', Coord { row = 1, col = 2 })
         , ('R', Coord { row = 1, col = 3 })
         , ('T', Coord { row = 1, col = 4 })
         , ('Y', Coord { row = 1, col = 5 })
         , ('U', Coord { row = 1, col = 6 })
         , ('I', Coord { row = 1, col = 7 })
         , ('O', Coord { row = 1, col = 8 })
         , ('P', Coord { row = 1, col = 9 })
         -- third row
         , ('A', Coord { row = 2, col = 0 })
         , ('S', Coord { row = 2, col = 1 })
         , ('D', Coord { row = 2, col = 2 })
         , ('F', Coord { row = 2, col = 3 })
         , ('G', Coord { row = 2, col = 4 })
         , ('H', Coord { row = 2, col = 5 })
         , ('J', Coord { row = 2, col = 6 })
         , ('K', Coord { row = 2, col = 7 })
         , ('L', Coord { row = 2, col = 8 })
         , (';', Coord { row = 2, col = 9 })
         -- fourth row
         , ('Z', Coord { row = 3, col = 0 })
         , ('X', Coord { row = 3, col = 1 })
         , ('C', Coord { row = 3, col = 2 })
         , ('V', Coord { row = 3, col = 3 })
         , ('B', Coord { row = 3, col = 4 })
         , ('N', Coord { row = 3, col = 5 })
         , ('M', Coord { row = 3, col = 6 })
         , (',', Coord { row = 3, col = 7 })
         , ('.', Coord { row = 3, col = 8 })
         , ('/', Coord { row = 3, col = 9 })
         ]

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
