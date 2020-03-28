{-# LANGUAGE NamedFieldPuns #-}
module Main
  ( main
  )where

--------------------------------------------------------------------------------
-- Dependencies
--------------------------------------------------------------------------------

import qualified Data.Maybe as Maybe
import Data.Time.Calendar as Calendar

-- I'm running this as a systemd timer that runs once per minute.

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

-- create the current /etc/hosts file
-- schedule the script to run again at the next relevant time

isToday :: Calendar.DayOfWeek -> Bool
isToday Monday = True
isToday _      = False

isAllowed :: [Allowance] -> Bool
isAllowed [] = False
isAllowed xs = do
  let rules = filter (isToday . day) xs
  case rules of
    [day] -> True
    []    -> False
    -- Error when more than one rule per day
    _     -> False

serializeEntry :: EtcHostEntry -> String
serializeEntry EtcHostEntry{ip, domains} =
  (getIPAddress ip) ++ "\t" ++ (unwords $ fmap getDomain domains)

toEtcHostEntry :: Rule -> Maybe EtcHostEntry
toEtcHostEntry Rule{urls, allowed} =
  if isAllowed allowed then
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
         , allowed = [ Allowance { day = Tuesday
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
  let etcHosts = unlines . fmap serializeEntry . Maybe.catMaybes $ fmap toEtcHostEntry rules
  putStrLn etcHosts
