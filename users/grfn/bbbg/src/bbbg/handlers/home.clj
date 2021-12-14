(ns bbbg.handlers.home
  (:require
   [bbbg.handlers.core :refer [page-response]]
   [compojure.core :refer [GET routes]]))

(defn- home-page []
  [:nav.home-nav
   [:ul
    [:li [:a {:href "/signup-forms"}
          "Event Signup Form"]]
    [:li [:a {:href "/login"}
          "Sign In"]]]])

(defn home-routes [_env]
  (routes
   (GET "/" []
     (page-response (home-page)))))
