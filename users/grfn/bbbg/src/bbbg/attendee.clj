(ns bbbg.attendee
  (:require [clojure.spec.alpha :as s]))

(s/def ::id uuid?)
