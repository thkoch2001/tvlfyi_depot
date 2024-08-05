{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wall #-}

module Main where

import qualified Data.ByteString.Lazy as Bytes.Lazy
import qualified Data.CaseInsensitive as CaseInsensitive
import qualified Data.Default as Default
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import ExecHelpers (dieUserError, CurrentProgramName)
import MyPrelude
import qualified System.Environment as Env
import Text.ICalendar
import Prelude hiding (log)

main :: IO ()
main = do
  Env.getArgs >>= \case
    [] -> dieUserError progName "First argument must be the ics file name"
    (file : _) ->
      do
        parse file
        >>= traverse_
          ( \vcal ->
              vcal
                & stripSingleTimezone
                & minify
                & printICalendar Default.def
                & Bytes.Lazy.putStr
          )

progName :: CurrentProgramName
progName = "ical-smolify"

log :: Error -> IO ()
log err = do
  putStderrLn (errorContext "ical-smolify" err & prettyError)

parse :: FilePath -> IO [VCalendar]
parse file = do
  parseICalendarFile Default.def file >>= \case
    Left err -> do
      dieUserError progName [fmt|Cannot parse ical file: {err}|]
    Right (cals, warnings) -> do
      for_ warnings (\warn -> log [fmt|Warning: {warn}|])
      pure cals

-- | Converts a single timezone definition to the corresponding X-WR-Timezone field.
stripSingleTimezone :: VCalendar -> VCalendar
stripSingleTimezone vcal =
  case vcal & vcTimeZones & Map.toList of
    [] -> vcal
    [(_, tz)] -> do
      let xtz =
            OtherProperty
              { otherName = CaseInsensitive.mk "X-WR-TIMEZONE",
                otherValue = tz & vtzId & tzidValue & textToBytesUtf8Lazy,
                otherParams = OtherParams Set.empty
              }
      vcal
        { vcOther =
            vcal & vcOther
              -- remove any existing x-wr-timezone fields
              & Set.filter (\prop -> (prop & otherName) /= (xtz & otherName))
              & Set.insert xtz,
          vcTimeZones = Map.empty
        }
    _more -> vcal

-- | Minify the vcalendar event by throwing away everything that’s not an event.
minify :: VCalendar -> VCalendar
minify vcal =
  vcal
    { vcProdId = ProdId "" (OtherParams Set.empty),
      -- , vcVersion    :: ICalVersion
      -- , vcScale      :: Scale
      -- , vcMethod     :: Maybe Method
      -- , vcOther      :: …
      -- , vcTimeZones  :: Map Text VTimeZone
      vcEvents = Map.map minifyEvent (vcal & vcEvents),
      vcTodos = Map.empty,
      vcJournals = Map.empty,
      vcFreeBusys = Map.empty,
      vcOtherComps = Set.empty
    }

minifyEvent :: VEvent -> VEvent
minifyEvent vev =
  vev
--  { veDTStamp       :: DTStamp
--   , veUID           :: UID
--   , veClass         :: Class -- ^ 'def' = 'Public'
--   , veDTStart       :: Maybe DTStart
--   , veCreated       :: Maybe Created
--   , veDescription   :: Maybe Description
--   , veGeo           :: Maybe Geo
--   , veLastMod       :: Maybe LastModified
--   , veLocation      :: Maybe Location
--   , veOrganizer     :: Maybe Organizer
--   , vePriority      :: Priority -- ^ 'def' = 0
--   , veSeq           :: Sequence -- ^ 'def' = 0
--   , veStatus        :: Maybe EventStatus
--   , veSummary       :: Maybe Summary
--   , veTransp        :: TimeTransparency -- ^ 'def' = 'Opaque'
--   , veUrl           :: Maybe URL
--   , veRecurId       :: Maybe RecurrenceId
--   , veRRule         :: Set RRule
--   , veDTEndDuration :: Maybe (Either DTEnd DurationProp)
--   , veAttach        :: Set Attachment
--   , veAttendee      :: Set Attendee
--   , veCategories    :: Set Categories
--   , veComment       :: Set Comment
--   , veContact       :: Set Contact
--   , veExDate        :: Set ExDate
--   , veRStatus       :: Set RequestStatus
--   , veRelated       :: Set RelatedTo
--   , veResources     :: Set Resources
--   , veRDate         :: Set RDate
--   , veAlarms        :: Set VAlarm
--   , veOther         :: Set OtherProperty
--   }
