--------------------------------------------------------------------------------
module Main where
--------------------------------------------------------------------------------
import RIO
import RIO.Text
import Prelude (putStrLn, read)
import Text.ParserCombinators.ReadP

import qualified Data.Time.Clock as Clock
--------------------------------------------------------------------------------

-- type Api = "run"
--            :> ReqBody '[JSON] Request
--            :> Post '[JSON] Response

data ShiftTimeRequest = ShiftTimeRequest
  { shiftSeconds :: Int
  , shiftMinutes :: Int
  , shiftHours :: Int
  , shiftDays :: Int
  , shiftWeeks :: Int
  , shiftMonths :: Int
  , shiftQuarters :: Int
  , shiftYears :: Int
  } deriving (Eq, Show)

defaultShiftTimeRequest :: ShiftTimeRequest
defaultShiftTimeRequest = ShiftTimeRequest
  { shiftSeconds = 0
  , shiftMinutes = 0
  , shiftHours = 0
  , shiftDays = 0
  , shiftWeeks = 0
  , shiftMonths = 0
  , shiftQuarters = 0
  , shiftYears = 0
  }

-- shiftTime :: Maybe Request -> IO Clock.UTCTime
-- shiftTime = Clock.getCurrentTime

data Unit = Second

digit :: ReadP Char
digit =
  satisfy (\c -> c >= '0' && c <= '9')

unit :: ReadP Unit
unit = do
  _ <- char 's'
  pure Second

request :: ReadP ShiftTimeRequest
request = do
  negative <- option Nothing $ fmap Just (satisfy (== '-'))
  n <- read <$> many1 digit
  _ <- unit
  case negative of
    Nothing -> pure $ defaultShiftTimeRequest { shiftSeconds = n }
    Just _  -> pure $ defaultShiftTimeRequest { shiftSeconds = -1 * n }

parseTime :: Text -> Maybe ShiftTimeRequest
parseTime x =
  case readP_to_S request (unpack x) of
    [(res, "")] -> Just res
    _ -> Nothing

main :: IO ()
main = putStrLn "Working!"
