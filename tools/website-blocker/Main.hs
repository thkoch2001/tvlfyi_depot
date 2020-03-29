{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveGeneric #-}
module Main
  ( main
  , getRules
  , URL(..)
  , Rule(..)
  ) where

--------------------------------------------------------------------------------
-- Dependencies
--------------------------------------------------------------------------------

import qualified Data.Maybe as Maybe
import qualified Data.Time.Clock as Clock
import qualified Data.Time.Calendar as Calendar
import qualified Data.Time.LocalTime as LocalTime
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Aeson as Aeson
import qualified Data.Either.Combinators as Either
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import qualified Data.Text.Read as TextRead
import qualified Data.List as List

import GHC.Generics
import Data.Aeson ((.:))
import Data.Text (Text)

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

newtype URL = URL { getURL :: Text } deriving (Show, Eq, Generic)

newtype IPAddress = IPAddress { getIPAddress :: Text } deriving (Show)

newtype Domain = Domain { getDomain :: Text } deriving (Show)

newtype Hour = Hour { getHour :: Int } deriving (Show, Eq, Generic)

newtype Minute = Minute { getMinute :: Int } deriving (Show, Eq, Generic)

data EtcHostEntry = EtcHostEntry { ip :: IPAddress
                                 , domains :: [Domain]
                                 } deriving (Show)

-- | Write these in terms of your system's local time (i.e. `date`).
data TimeSlot = TimeSlot { beg :: (Hour, Minute)
                         , end :: (Hour, Minute)
                         } deriving (Show, Eq, Generic)

data Allowance = Allowance { day :: Calendar.DayOfWeek
                           , timeslots :: [TimeSlot]
                           } deriving (Show, Eq, Generic)

data Rule = Rule { urls :: [URL]
                 , allowed :: [Allowance]
                 } deriving (Show, Eq, Generic)

--------------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------------

instance Aeson.FromJSON TimeSlot where
  parseJSON = Aeson.withText "timeslot" $ \x -> do
    let [a, b] = Text.splitOn "-" x
        [ah, am] = Text.splitOn ":" a
        [bh, bm] = Text.splitOn ":" b
    case extractTimeSlot ah am bh bm of
      Left s  -> fail s
      Right x -> pure x
    where
      extractTimeSlot :: Text -> Text -> Text -> Text -> Either String TimeSlot
      extractTimeSlot ah am bh bm = do
        (begh, _) <- TextRead.decimal ah
        (begm, _) <- TextRead.decimal am
        (endh, _) <- TextRead.decimal bh
        (endm, _) <- TextRead.decimal bm
        pure $ TimeSlot{ beg = (Hour begh, Minute begm)
                       , end = (Hour endh, Minute endm)
                       }

instance Aeson.FromJSON Allowance where
  parseJSON = Aeson.withObject "allowance" $ \x -> do
    day <- x .: "day"
    timeslots <- x .: "timeslots"
    pure $ Allowance{day, timeslots}

instance Aeson.FromJSON URL where
  parseJSON = Aeson.withText "URL" $ \x -> do
    pure $ URL { getURL = x }

instance Aeson.FromJSON Rule where
  parseJSON = Aeson.withObject "rule" $ \x -> do
    urls <- x .: "urls"
    allowed <- x .: "allowed"
    pure Rule{urls, allowed}

--------------------------------------------------------------------------------
-- Functions
--------------------------------------------------------------------------------

isWithinTimeSlot :: LocalTime.LocalTime -> [TimeSlot] -> Bool
isWithinTimeSlot date timeslots =
  List.any withinTimeSlot timeslots
  where
    withinTimeSlot :: TimeSlot -> Bool
    withinTimeSlot TimeSlot{ beg = (Hour ah, Minute am)
                           , end = (Hour bh, Minute bm)
                           } =
      let LocalTime.TimeOfDay{LocalTime.todHour, LocalTime.todMin} =
            LocalTime.localTimeOfDay date
      in (todHour > ah) && (todMin > am) && (todHour < bh) && (todMin < bm)

isToday :: LocalTime.LocalTime -> Calendar.DayOfWeek -> Bool
isToday date day = Calendar.dayOfWeek (LocalTime.localDay date) == day

isAllowed :: LocalTime.LocalTime -> [Allowance] -> Bool
isAllowed _ [] = False
isAllowed date allowances = do
  case filter (isToday date . day) allowances of
    [Allowance{timeslots}] ->
      isWithinTimeSlot date timeslots
    [] -> False
    -- Error when more than one rule per day
    _  -> False

serializeEntry :: EtcHostEntry -> Text
serializeEntry EtcHostEntry{ip, domains} =
  (getIPAddress ip) <> "\t" <> (Text.unwords $ fmap getDomain domains)

toEtcHostEntry :: LocalTime.LocalTime -> Rule -> Maybe EtcHostEntry
toEtcHostEntry date Rule{urls, allowed} =
  if isAllowed date allowed then
    Nothing
  else
    Just $ EtcHostEntry { ip = IPAddress "127.0.0.1"
                        , domains = fmap (Domain . getURL) urls
                        }

getRules :: IO [Rule]
getRules = do
  contents <- LazyByteString.readFile "rules.json"
  let payload = Aeson.eitherDecode contents
  pure $ Either.fromRight [] payload

header :: Text
header =
  Text.unlines [ "################################################################################"
               , "# Added by url-blocker"
               , "################################################################################"
               ]

main :: IO ()
main = do
  rules <- getRules
  tz <- LocalTime.getCurrentTimeZone
  ct <- Clock.getCurrentTime
  let date = LocalTime.utcToLocalTime tz ct
      etcHosts = Text.unlines . fmap serializeEntry . Maybe.catMaybes $ fmap (toEtcHostEntry date) rules
  existingEtcHosts <- TextIO.readFile "/etc/hosts"
  TextIO.putStrLn $ existingEtcHosts <> "\n" <> header <> "\n" <> etcHosts
