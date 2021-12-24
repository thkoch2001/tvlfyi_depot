(ns bbbg.meetup-user
  (:require [clojure.spec.alpha :as s]))

(s/def ::id
  (s/nilable
   (s/and string?
          seq
          #(re-matches #"\d+" %))))
