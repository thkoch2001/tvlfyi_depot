(ns bbbg.styles
  (:require [garden.def :refer [defstyles]]
            [garden.compiler :refer [compile-css]]))

(defstyles styles
  )

(def stylesheet
  (compile-css styles))
