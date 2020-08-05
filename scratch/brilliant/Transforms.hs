--------------------------------------------------------------------------------
module Transforms where
--------------------------------------------------------------------------------
import Control.Applicative ((<|>))
import Text.ParserCombinators.ReadP
--------------------------------------------------------------------------------

data Transform = VerticalFlip
               | HorizontalFlip
               | Shift Int
               deriving (Eq, Show)

digit :: ReadP Char
digit =
  satisfy (\c -> c >= '0' && c <= '9')

command :: ReadP Transform
command = vertical
      <|> horizontal
      <|> shift
  where
    vertical =
      char 'V' >> pure VerticalFlip

    horizontal =
      char 'H' >> pure HorizontalFlip

    shift = do
      _ <- char 'S'
      negative <- option Nothing $ fmap Just (satisfy (== '-'))
      n <- read <$> many1 digit
      case negative of
        Nothing -> pure $ Shift n
        Just _  -> pure $ Shift (-1 * n)

fromString :: String -> Maybe [Transform]
fromString x =
  case readP_to_S (manyTill command eof) x of
   [(res, "")] -> Just res
   _           -> Nothing
