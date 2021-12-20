(ns bbbg.attendee-check
  (:require [clojure.spec.alpha :as s]))

(s/def ::id uuid?)
