{-# LANGUAGE ViewPatterns #-}
--------------------------------------------------------------------------------
module Xanthous.Command where
--------------------------------------------------------------------------------
import Xanthous.Prelude hiding (Left, Right, Down)
--------------------------------------------------------------------------------
import Graphics.Vty.Input (Key(..), Modifier(..))
--------------------------------------------------------------------------------
import Xanthous.Data (Direction(..))
--------------------------------------------------------------------------------

data Command
  = Quit
  | Move Direction
  | PreviousMessage
  | PickUp
  | Open

commandFromKey :: Key -> [Modifier] -> Maybe Command
commandFromKey (KChar 'q') [] = Just Quit
commandFromKey (KChar (directionFromChar -> Just dir)) [] = Just $ Move dir
commandFromKey (KChar 'p') [MCtrl] = Just PreviousMessage
commandFromKey (KChar ',') [] = Just PickUp
commandFromKey (KChar 'o') [] = Just Open
commandFromKey _ _ = Nothing

--------------------------------------------------------------------------------

directionFromChar :: Char -> Maybe Direction
directionFromChar 'h' = Just Left
directionFromChar 'j' = Just Down
directionFromChar 'k' = Just Up
directionFromChar 'l' = Just Right
directionFromChar 'y' = Just UpLeft
directionFromChar 'u' = Just UpRight
directionFromChar 'b' = Just DownLeft
directionFromChar 'n' = Just DownRight
directionFromChar '.' = Just Here
directionFromChar _   = Nothing
