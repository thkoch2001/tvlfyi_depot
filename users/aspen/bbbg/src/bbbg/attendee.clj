(ns bbbg.attendee
  (:require [clojure.spec.alpha :as s]))

(s/def ::id uuid?)

(s/def ::meetup-name (s/and string? seq))

(s/def ::discord-name (s/nilable string?))

(s/def ::organizer-notes string?)
