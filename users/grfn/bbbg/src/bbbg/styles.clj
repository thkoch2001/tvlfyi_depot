;; -*- eval: (rainbow-mode) -*-
(ns bbbg.styles
  (:require
   [garden.compiler :refer [compile-css]]
   [garden.def :refer [defstyles]]
   [garden.selectors :refer [attr= visited hover active & descendant]]))

(def black "#342e37")

(def silver "#f9fafb")

(def gray "#aaa")

(def gray-light "#ddd")

(def purple "#837aff")

(def red "#c42348")

(def orange "#fa824c")

(def yellow "#FACB0F")

(def blue "#026fb1")

(def green "#BEEF9E")

(def contextual
  {:success green
   :info blue
   :warning yellow
   :error red})

;;;

(defstyles global-nav
  [:.global-nav
   {:background-color silver}

   [:>ul
    {:display :flex
     :flex-direction :row
     :list-style :none}]

   [:a (descendant :.link-form (attr= "type" "submit"))
    {:padding "1rem 1.5rem"
     :display :block
     :color black
     :text-decoration :none}

     [(& hover)
      {:color blue}]]

   [:.spacer
    {:flex 1}]])

(def link-conditional-styles
  (list
   [(& hover) (& active)
    {:text-decoration :underline}]
   [(& active)
    {:color purple}]))

(defstyles link-form
  [:form.link-form
   {:margin 0}
   [(attr= "type" "submit")
    {:background "none"
     :border "none"
     :padding 0
     :color blue
     :text-decoration :none
     :cursor :pointer}
    link-conditional-styles]])

(defstyles styles
  global-nav
  link-form

  [:body
   {:color black}]

  [:a {:color blue
       :text-decoration :none}
   link-conditional-styles])

(def stylesheet
  (compile-css styles))
