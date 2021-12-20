(ns bbbg.util.time
  "Utilities for dealing with date/time"
  (:require [clojure.spec.alpha :as s]
            [clojure.test.check.generators :as gen]
            [java-time :as jt])
  (:import [java.time
            LocalDateTime LocalTime OffsetDateTime ZoneId ZoneOffset
            LocalDate Year]
           [java.time.format DateTimeFormatter DateTimeParseException]
           java.util.Calendar
           org.apache.commons.lang3.time.DurationFormatUtils))

(set! *warn-on-reflection* true)

(defprotocol ToOffsetDateTime
  (->OffsetDateTime [this]
    "Coerces its argument to a `java.time.OffsetDateTime`"))

(extend-protocol ToOffsetDateTime
  OffsetDateTime
  (->OffsetDateTime [odt] odt)

  java.util.Date
  (->OffsetDateTime [d]
    (-> d
        .toInstant
        (OffsetDateTime/ofInstant (ZoneId/of "UTC")))))

(defprotocol ToLocalTime (->LocalTime [this]))
(extend-protocol ToLocalTime
  LocalTime
  (->LocalTime [lt] lt)

  java.sql.Time
  (->LocalTime [t]
    (let [^Calendar cal (doto (Calendar/getInstance)
                          (.setTime t))]
      (LocalTime/of
       (.get cal Calendar/HOUR_OF_DAY)
       (.get cal Calendar/MINUTE)
       (.get cal Calendar/SECOND))))

  java.util.Date
  (->LocalTime [d]
    (-> d .toInstant (LocalTime/ofInstant (ZoneId/of "UTC")))))

(defn local-time? [x] (satisfies? ToLocalTime x))
(s/def ::local-time
  (s/with-gen local-time?
    #(gen/let [hour (gen/choose 0 23)
               minute (gen/choose 0 59)
               second (gen/choose 0 59)
               nanos gen/nat]
       (LocalTime/of hour minute second nanos))))

(defprotocol ToLocalDate (->LocalDate [this]))
(extend-protocol ToLocalDate
  LocalDate
  (->LocalDate [ld] ld)

  java.util.Date
  (->LocalDate [d]
    (-> d .toInstant (LocalDate/ofInstant (ZoneId/of "UTC")))))

(defn local-date? [x] (satisfies? ToLocalDate x))
(s/def ::local-date
  (s/with-gen local-date?
    #(gen/let [year (gen/choose Year/MIN_VALUE Year/MAX_VALUE)
               day (gen/choose 1 (if (.isLeap (Year/of year))
                                   366
                                   365))]
       (LocalDate/ofYearDay year day))))

(extend-protocol Inst
  OffsetDateTime
  (inst-ms* [zdt]
    (inst-ms* (.toInstant zdt)))

  LocalDateTime
  (inst-ms* [^LocalDateTime ldt]
    (inst-ms* (.toInstant ldt ZoneOffset/UTC))))

(let [formatter DateTimeFormatter/ISO_OFFSET_DATE_TIME]
  (defn ^OffsetDateTime parse-iso-8601
    "Parse s as an iso-8601 datetime, returning nil if invalid"
    [^String s]
    (try
      (OffsetDateTime/parse s formatter)
      (catch DateTimeParseException _ nil)))

  (defn format-iso-8601
    "Format dt, which can be an OffsetDateTime or java.util.Date, as iso-8601"
    [dt]
    (some->> dt ->OffsetDateTime (.format formatter))))

(let [formatter DateTimeFormatter/ISO_TIME]
  (defn parse-iso-8601-time
    "Parse s as an iso-8601 timestamp, returning nil if invalid"
    [^String s]
    (try
      (LocalTime/parse s formatter)
      (catch DateTimeParseException _ nil)))

  (defn format-iso-8601-time
    "Format lt, which can be a LocalTime or java.sql.Time, as an iso-8601
    formatted timestamp without a date."
    [lt]
    (some->> lt ->LocalTime (.format formatter))))

(defmethod print-dup LocalTime [t w]
  (binding [*out* w]
    (print "#local-time ")
    (print (str "\"" (format-iso-8601-time t) "\""))))

(defmethod print-method LocalTime [t w]
  (print-dup t w))

(let [formatter DateTimeFormatter/ISO_LOCAL_DATE]
  (defn parse-iso-8601-date
    "Parse s as an iso-8601 date, returning nil if invalid"
    [^String s]
    (try
      (LocalDate/parse s formatter)
      (catch DateTimeParseException _ nil)))

  (defn format-iso-8601-date
    "Format lt, which can be a LocalDate, as an iso-8601 formatted date without
    a timestamp."
    [lt]
    (some->> lt ->LocalDate (.format formatter))))

(defmethod print-dup LocalDate [t w]
  (binding [*out* w]
    (print "#local-date ")
    (print (str "\"" (format-iso-8601-date t) "\""))))

(defmethod print-method LocalDate [t w]
  (print-dup t w))


(defn ^String human-format-duration
  "Human-format the given duration"
  [^java.time.Duration dur]
  (DurationFormatUtils/formatDurationWords (Math/abs (.toMillis dur)) true true))

(comment
  (human-format-duration (jt/hours 5))
  (human-format-duration (jt/plus (jt/hours 5) (jt/minutes 7)))
  )
