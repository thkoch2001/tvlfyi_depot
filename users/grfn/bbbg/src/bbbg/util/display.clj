(ns bbbg.util.display
  (:require
   [bbbg.util.time :as t])
  (:import
   [java.time.format DateTimeFormatter FormatStyle]))

(defn format-date
  ([d] (format-date d FormatStyle/MEDIUM))
  ([d ^FormatStyle format-style]
   (let [formatter (DateTimeFormatter/ofLocalizedDate format-style)]
     (.format (t/->LocalDate d) formatter))))

(comment
  (format-date #inst "2021-12-19T05:00:00.000-00:00")
  (format-date #inst "2021-12-19T05:00:00.000-00:00"
               FormatStyle/SHORT)
  )
