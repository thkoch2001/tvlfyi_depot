{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
--------------------------------------------------------------------------------
module Main where
--------------------------------------------------------------------------------
import RIO hiding (Handler)
import RIO.Text
import RIO.Time
import Servant
import Data.Time.Clock.POSIX
import Prelude (read)
import Text.ParserCombinators.ReadP

import qualified Network.Wai.Handler.Warp as Warp
--------------------------------------------------------------------------------

type Api = "run"
           :> QueryParam' '[Required] "offset" Text
           :> Get '[JSON] UTCTime

server :: Server Api
server = compute
  where
    compute :: Text -> Handler UTCTime
    compute x = do
      case parseInput x of
        Nothing -> throwError err401
        Just req -> do
          res <- liftIO $ shiftTime req
          pure res

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

instance Semigroup ShiftTimeRequest where
  (ShiftTimeRequest as am ah ad aw amonths aq ay) <> (ShiftTimeRequest bs bm bh bd bw bmonths bq by) =
    ShiftTimeRequest
    { shiftSeconds = as + bs
    , shiftMinutes = am + bm
    , shiftHours = ah + bh
    , shiftDays = ad + bd
    , shiftWeeks = aw + bw
    , shiftMonths = amonths + bmonths
    , shiftQuarters = aq + bq
    , shiftYears = ay + by
    }

instance Monoid ShiftTimeRequest where
  mempty = defaultShiftTimeRequest

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

-- This basically broken because it doesn't account for:
-- Exhales... time stuff
--   - Leap seconds, leap days, leap years...
--   - Months like February having 28 days and others having 31
--   - other things that I'm probably not considering
toSeconds :: ShiftTimeRequest -> NominalDiffTime
toSeconds ShiftTimeRequest{..} = do
  let minutes = 60
      hours = minutes * 60
      days = hours * 24
      weeks = days * 7
      months = weeks * 4
      quarters = months * 3
      years = days * 365
  fromIntegral $ shiftSeconds +
    shiftMinutes * minutes +
    shiftHours * hours +
    shiftDays * days +
    shiftWeeks * weeks +
    shiftMonths * months +
    shiftQuarters * quarters +
    shiftYears * years

shiftTime :: ShiftTimeRequest -> IO UTCTime
shiftTime req = do
  t <- getPOSIXTime
  let t' = t + toSeconds req
  pure $ posixSecondsToUTCTime t'

data Unit = Second
          | Minute
          | Hour
          | Day
          | Week
          | Month
          | Quarter
          | Year
  deriving (Eq, Show)

digit :: ReadP Char
digit =
  satisfy (\c -> c >= '0' && c <= '9')

unit :: ReadP Unit
unit = do
  c <- get
  case c of
    's' -> pure Second
    'm' -> pure Minute
    'h' -> pure Hour
    'd' -> pure Day
    'w' -> pure Week
    'M' -> pure Month
    'q' -> pure Quarter
    'y' -> pure Year
    _ -> fail $ "We don't support this unit: " ++ show c

request :: ReadP ShiftTimeRequest
request = do
  negative <- option Nothing $ fmap Just (satisfy (== '-'))
  n <- read <$> many1 digit
  u <- unit
  let amt = if isJust negative then -1 * n else n
  case u of
    Second  -> pure $ defaultShiftTimeRequest { shiftSeconds = amt }
    Minute  -> pure $ defaultShiftTimeRequest { shiftMinutes = amt }
    Hour    -> pure $ defaultShiftTimeRequest { shiftHours = amt }
    Day     -> pure $ defaultShiftTimeRequest { shiftDays = amt }
    Week    -> pure $ defaultShiftTimeRequest { shiftWeeks = amt }
    Month   -> pure $ defaultShiftTimeRequest { shiftMonths = amt }
    Quarter -> pure $ defaultShiftTimeRequest { shiftQuarters = amt }
    Year    -> pure $ defaultShiftTimeRequest { shiftYears = amt }

parseInput :: Text -> Maybe ShiftTimeRequest
parseInput x =
  case readP_to_S (manyTill request eof) (unpack x) of
    [(xs, "")] -> Just $ mconcat xs
    _ -> Nothing

main :: IO ()
main = Warp.run 8000 $ serve (Proxy @ Api) server
