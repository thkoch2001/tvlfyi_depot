{-# LANGUAGE NamedFieldPuns #-}
module Main (main) where

--------------------------------------------------------------------------------
-- Dependencies
--------------------------------------------------------------------------------

import qualified Data.Maybe as Maybe
import qualified Data.Time.Clock as Clock
import qualified Data.Time.Calendar as Calendar

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

newtype URL = URL { getURL :: String } deriving (Show)

newtype IPAddress = IPAddress { getIPAddress :: String } deriving (Show)

newtype Domain = Domain { getDomain :: String } deriving (Show)

newtype Hour = Hour { getHour :: Integer }

newtype Minute = Minute { getMinute :: Integer }

data EtcHostEntry = EtcHostEntry { ip :: IPAddress
                                 , domains :: [Domain]
                                 } deriving (Show)

data TimeRange = TimeRange { beg :: (Hour, Minute)
                           , end :: (Hour, Minute)
                           }

data Allowance = Allowance { day :: Calendar.DayOfWeek
                           , timeslots :: [TimeRange]
                           }

data Rule = Rule { urls :: [URL]
                 , allowed :: [Allowance]
                 }

--------------------------------------------------------------------------------
-- Functions
--------------------------------------------------------------------------------

isToday :: Clock.UTCTime -> Calendar.DayOfWeek -> Bool
isToday date day = Calendar.dayOfWeek (Clock.utctDay date) == day

isAllowed :: Clock.UTCTime -> [Allowance] -> Bool
isAllowed _ [] = False
isAllowed date xs = do
  let rules = filter (isToday date . day) xs
  case rules of
    [day] -> True
    []    -> False
    -- Error when more than one rule per day
    _     -> False

serializeEntry :: EtcHostEntry -> String
serializeEntry EtcHostEntry{ip, domains} =
  (getIPAddress ip) ++ "\t" ++ (unwords $ fmap getDomain domains)

toEtcHostEntry :: Clock.UTCTime -> Rule -> Maybe EtcHostEntry
toEtcHostEntry date Rule{urls, allowed} =
  if isAllowed date allowed then
    Nothing
  else
    Just $ EtcHostEntry { ip = IPAddress "127.0.0.1"
                        , domains = fmap (Domain . getURL) urls
                        }

-- | Location of the rules.json file.
rulesFile :: FilePath
rulesFile =
  "~/.config/website-blocker/rules.json"

-- | Reads and parses JSON from `rulesFile` and returns the result.
getRules :: IO [Rule]
getRules = pure $
  [ Rule { urls = [ URL "facebook.com"
                  , URL "twitter.com"
                  , URL "youtube.com"
                  , URL "instagram.com"
                  ]
         , allowed = []
         }
  , Rule { urls = [ URL "chat.googleplex.com" ]
         , allowed = [ Allowance { day = Calendar.Saturday
                                 , timeslots = [ TimeRange { beg = (Hour 0, Minute 0)
                                                           , end = (Hour 0, Minute 0)
                                                           }
                                               ]
                                 }
                     ]
         }
  ]

main :: IO ()
main = do
  rules <- getRules
  date <- Clock.getCurrentTime
  let etcHosts = unlines . fmap serializeEntry . Maybe.catMaybes $ fmap (toEtcHostEntry date) rules
  putStrLn etcHosts
