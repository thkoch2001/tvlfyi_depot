;; -*- eval: (rainbow-mode) -*-
(ns bbbg.styles
  (:require
   [garden.color :as color]
   [garden.compiler :refer [compile-css]]
   [garden.def :refer [defstyles]]
   [garden.selectors :refer [& active attr= descendant hover]]
   [garden.stylesheet :refer [at-media]]
   [garden.units :refer [px]]))

(def black "#342e37")

(def silver "#f9fafb")

(def gray "#aaa")

(def gray-light "#ddd")

(def purple "#837aff")

(def red "#c42348")

(def orange "#fa824c")

(def yellow "#FACB0F")

(def blue "#026fb1")

(def green "#87E24B")

(def contextual-colors
  {:success green
   :info blue
   :warning yellow
   :error red})

;;;

(def content-width (px 1600))

(defn not-mobile [& rules]
  (at-media
   {:screen true
    :min-width content-width}
   [:&
    rules]))

;;;

(defstyles global-nav
  [:.global-nav
   {:background-color silver}

   [:>ul
    {:display :flex
     :flex-direction :row
     :list-style :none}

    (not-mobile
     {:width content-width
      :margin "0 auto"})]

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

(defstyles flash
  [:.flash-messages
   {:width "800px"
    :margin "1rem auto"}]

  [:.flash-message
   {:padding "1rem 1.5rem"
    :border "1px solid"
    :margin-bottom "1rem"}]

  (for [[context color] contextual-colors]
    [(& (keyword (str ".flash-" (name context))))
     {:border-color color
      :background-color (color/lighten color 30)
      :border-radius "5px"}]))

(defstyles styles
  global-nav
  link-form
  flash

  [:body
   {:color black}]

  [:a {:color blue
       :text-decoration :none}
   link-conditional-styles])

(def stylesheet
  (compile-css styles))
