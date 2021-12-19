(ns bbbg.handlers.core
  (:require
   [bbbg.user :as user]
   [bbbg.views.flash :as flash]
   [hiccup.core :refer [html]]
   [ring.util.response :refer [content-type response]]))

(defn render-page [opts & body]
  (let [[{:keys [title]} body]
        (if (map? opts)
          [opts body]
          [{} (concat [opts] body)])]
    (html
     [:html {:lang "en"}
      [:head
       [:meta {:charset "UTF-8"}]
       [:title (if title
                 (str title " - BBBG")
                 "BBBG")]
       [:link {:rel "stylesheet"
               :type "text/css"
               :href "/main.css"}]]
      [:body
       [:div.content
        (flash/render-flash)
        body]
       [:script {:src "https://cdnjs.cloudflare.com/ajax/libs/tarekraafat-autocomplete.js/10.2.6/autoComplete.js"}]
       [:script {:src "/main.js"}]]])))

(defn page-response [& render-page-args]
  (-> (apply render-page render-page-args)
      response
      (content-type "text/html")))

(defn authenticated? [request]
  (some? (get-in request [:session ::user/id])))

(comment
  (render-page
   [:h1 "hi"])
  )
