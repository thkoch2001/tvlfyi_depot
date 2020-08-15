{-# LANGUAGE DeriveGeneric #-}
--------------------------------------------------------------------------------
module Main where
--------------------------------------------------------------------------------
import Data.Hashable
import Data.Function ((&))
import GHC.Generics
import Text.ParserCombinators.ReadP
import Control.Applicative

import qualified Data.HashSet as HS
--------------------------------------------------------------------------------

data Direction
  = DirLeft
  | DirRight
  | DirUp
  | DirDown
  deriving (Eq, Show)

data Point = Point Int Int
  deriving (Eq, Show, Ord, Generic)
instance Hashable Point

data Orientation
  = Horizontal
  | Vertical
  deriving (Eq, Show)

data Anchor
  = Beg
  | End
  deriving (Eq, Show)

data Rotation
  = CW
  | CCW
  deriving (Eq, Show)

data Line = Line Point Point
  deriving (Show, Generic)
instance Hashable Line

instance Eq Line where
  Line begA endA == Line begB endB =
    (begA == begB && endA == endB) ||
    (begA == endB && endA == begB)

data Game = Game (HS.HashSet Line) [Line]
  deriving (Eq, Show)

data Scoreboard = Scoreboard Int Int
  deriving (Eq)

instance Semigroup Scoreboard where
  (Scoreboard a b) <> (Scoreboard x y) =
    Scoreboard (a + x) (b + y)

instance Monoid Scoreboard where
  mempty = Scoreboard 0 0

data Turn
  = Player1
  | Player2
  deriving (Eq, Show)

next :: Turn -> Turn
next Player1 = Player2
next Player2 = Player1

instance Show Scoreboard where
  show (Scoreboard p1 p2) =
    "Player 1: " ++ show (p1) ++ " Player 2: " ++ show (p2)

digit :: ReadP Char
digit = satisfy (\c -> c >= '0' && c <= '9')

int :: ReadP Int
int = read <$> many1 digit

line :: ReadP String
line = manyTill get (char '\n')

direction :: ReadP Direction
direction = do
  c <- char 'L' <|> char 'R' <|> char 'U' <|> char 'D'
  case c of
    'L' -> pure DirLeft
    'R' -> pure DirRight
    'U' -> pure DirUp
    'D' -> pure DirDown

validMove :: Int -> Int -> ReadP Line
validMove w h = do
  x <- int
  skipSpaces
  y <- int
  skipSpaces
  dir <- direction
  char '\n'
  if x >= 0 && x <= w &&  y >= 0 && y <= h then do
    let beg = Point x y
    pure $ mkLine beg (shiftPoint dir beg)
  else
    fail "Expected a move on the game board"

game :: ReadP Game
game = do
  w <- read <$> line :: ReadP Int
  h <- read <$> line :: ReadP Int
  locs <- read <$> line :: ReadP Int
  moves <- count locs (validMove w h)
  eof
  pure $ Game mempty moves

parseInput :: String -> Maybe Game
parseInput x = do
  case readP_to_S game x of
    [(res, "")] -> Just res
    _ -> Nothing

-- | Smart constructor to ensure that beg is always < end.
mkLine :: Point -> Point -> Line
mkLine beg end =
  if beg < end then Line beg end else Line end beg

mkLineDir :: Int -> Int -> Direction -> Line
mkLineDir x y dir =
  let beg = Point x y
  in mkLine beg (shiftPoint dir beg)

mkLineDir' :: Point -> Direction -> Line
mkLineDir' (Point x y) dir = mkLineDir x y dir

shiftPoint :: Direction -> Point -> Point
shiftPoint DirLeft  (Point x y) = Point (x - 1) y
shiftPoint DirRight (Point x y) = Point (x + 1) y
shiftPoint DirUp    (Point x y) = Point x (y + 1)
shiftPoint DirDown  (Point x y) = Point x (y - 1)

shiftLine :: Direction -> Line -> Line
shiftLine dir (Line beg end) =
  mkLine (shiftPoint dir beg) (shiftPoint dir end)

rotateLine :: Anchor -> Rotation -> Line -> Line
rotateLine anchor rotation line =
  doRotateLine (classifyOrientation line) anchor rotation line

doRotateLine :: Orientation -> Anchor -> Rotation -> Line -> Line
doRotateLine Horizontal Beg CW  (Line beg _) = mkLineDir' beg DirDown
doRotateLine Horizontal Beg CCW (Line beg _) = mkLineDir' beg DirUp
doRotateLine Horizontal End CW  (Line _ end) = mkLineDir' end DirUp
doRotateLine Horizontal End CCW (Line _ end) = mkLineDir' end DirDown
doRotateLine Vertical   Beg CW  (Line beg _) = mkLineDir' beg DirRight
doRotateLine Vertical   Beg CCW (Line beg _) = mkLineDir' beg DirLeft
doRotateLine Vertical   End CW  (Line _ end) = mkLineDir' end DirLeft
doRotateLine Vertical   End CCW (Line _ end) = mkLineDir' end DirRight

classifyOrientation :: Line -> Orientation
classifyOrientation (Line (Point x1 y1) (Point x2 y2)) =
  if y1 == y2 then Horizontal else Vertical

closesAnySquare :: HS.HashSet Line -> Line -> Bool
closesAnySquare allMoves line = do
  let alreadyDrawn x = HS.member x allMoves
  case classifyOrientation line of
    Horizontal ->
      all alreadyDrawn
        [ shiftLine DirUp line
        , rotateLine Beg CCW line
        , rotateLine End CW line
        ] ||
      all alreadyDrawn
        [ shiftLine DirDown line
        , rotateLine Beg CW line
        , rotateLine End CCW line
        ]
    Vertical ->
      all alreadyDrawn
        [ shiftLine DirLeft line
        , rotateLine Beg CCW line
        , rotateLine End CW line
        ] ||
      all alreadyDrawn
        [ shiftLine DirRight line
        , rotateLine Beg CW line
        , rotateLine End CCW line
        ]

incScoreboard :: Turn -> Scoreboard -> Scoreboard
incScoreboard Player1 score = score <> Scoreboard 1 0
incScoreboard Player2 score = score <> Scoreboard 0 1

scoreGame :: Turn -> Game -> Scoreboard -> Maybe Scoreboard
scoreGame _ (Game _ []) score = Just $ score
scoreGame player (Game allMoves (line:rest)) score =
  if HS.member line allMoves then
    Nothing
  else do
    let allMoves' = HS.insert line allMoves
        score' = if closesAnySquare allMoves line then
                   incScoreboard player score
                 else score
    scoreGame (next player) (Game allMoves' rest) score'

(|>) :: a -> (a -> b) -> b
(|>) = (&)

main :: IO ()
main = do
  input <- readFile "game.txt"
  case parseInput input of
    Nothing -> putStrLn "invalid"
    Just game ->
      case scoreGame Player1 game mempty of
        Nothing -> putStrLn "invalid"
        Just score -> print score
