(ns bbbg.handlers.core
  (:require
   [bbbg.user :as user]
   [bbbg.views.flash :as flash]
   [hiccup.core :refer [html]]
   [ring.util.response :refer [content-type response]]))

(def ^:dynamic *authenticated?* false)

(defn authenticated? [request]
  (some? (get-in request [:session ::user/id])))

(defn wrap-auth-required [handler]
  (fn [req]
    (when (authenticated? req)
      (handler req))))

(defn wrap-dynamic-auth [handler]
  (fn [req]
    (binding [*authenticated?* (authenticated? req)]
      (handler req))))

(defn global-nav []
  [:nav.global-nav
   [:ul
    [:li [:a {:href "/events"}
          "Events"]]
    (when *authenticated?*
      [:li [:a {:href "/attendees"}
            "Attendees"]])
    [:li.spacer]
    [:li
     (if *authenticated?*
       [:form.link-form
        {:method :post
         :action "/auth/sign-out"}
        [:input {:type "submit"
                 :value "Sign Out"}]]
       [:a {:href "/auth/discord"}
        "Sign In"])]]])

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
        (global-nav)
        #_(flash/render-flash flash/test-flash)
        (flash/render-flash)
        body]
       [:script {:src "https://cdnjs.cloudflare.com/ajax/libs/tarekraafat-autocomplete.js/10.2.6/autoComplete.js"}]
       [:script {:src "/main.js"}]]])))

(defn page-response [& render-page-args]
  (-> (apply render-page render-page-args)
      response
      (content-type "text/html")))

(comment
  (render-page
   [:h1 "hi"])
  )
