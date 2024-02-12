(ns bbbg.event
  (:require [clojure.spec.alpha :as s]))

(s/def ::id uuid?)
