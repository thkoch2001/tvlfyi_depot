(ns bbbg.util.spec
  (:require [expound.alpha :as exp]
            [clojure.spec.alpha :as s]))

(defn assert!
  ([spec s] (assert! "Spec assertion failed" spec s))
  ([message spec x]
   (if (s/valid? spec x)
     x
     (throw (ex-info
             (str message
                  "\n"
                  (exp/expound-str spec x))
             (assoc (s/explain-data spec x)
                    ::s/failure
                    ::s/assertion-failed))))))
