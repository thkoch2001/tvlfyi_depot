(ns bbbg.user
  (:require [clojure.spec.alpha :as s]))

(s/def ::id uuid?)

(s/def ::discord-id string?)

(s/def ::username string?)
