(ns bbbg.event-attendee
  (:require [clojure.spec.alpha :as s]))

(s/def ::attended? boolean?)

(s/def ::rsvpd-attending? boolean?)
