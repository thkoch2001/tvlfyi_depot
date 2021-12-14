(ns bbbg.web
  (:require
   [bbbg.handlers.attendees :as attendees]
   [bbbg.handlers.events :as events]
   [bbbg.handlers.home :as home]
   [bbbg.handlers.signup-form :as signup-form]
   [bbbg.styles :refer [stylesheet]]
   [bbbg.util.core :as u]
   [bbbg.views.flash :refer [wrap-page-flash]]
   [clojure.spec.alpha :as s]
   [com.stuartsierra.component :as component]
   [compojure.core :refer [GET routes]]
   [config.core :refer [env]]
   [org.httpkit.server :as http-kit]
   [ring.middleware.flash :refer [wrap-flash]]
   [ring.middleware.keyword-params :refer [wrap-keyword-params]]
   [ring.middleware.params :refer [wrap-params]]
   [ring.middleware.session :refer [wrap-session]]
   [ring.middleware.session.cookie :refer [cookie-store]]
   [ring.util.response :refer [content-type resource-response response]])
  (:import
   java.util.Base64))

(s/def ::port pos-int?)

(s/def ::cookie-secret
  (s/and bytes? #(= 16 (count %))))

(s/def ::config
  (s/keys :req [::port]
          :opt [::cookie-secret]))

(s/fdef make-server
  :args (s/cat :config ::config))


(defn- string->cookie-secret [raw]
  (s/assert
   ::cookie-secret
   (when raw
     (.decode (Base64/getDecoder)
              (.getBytes raw "UTF-8")))))

(defn env->config []
  (s/assert
   ::config
   (u/remove-nils
    {::port (:port env 8888)
     ::cookie-secret (some-> env :cookie-secret string->cookie-secret)})))

(defn dev-config []
  (s/assert
   ::config
   {::port 8888
    ::cookie-secret (into-array Byte/TYPE (repeat 16 0))}))

;;;

(defn app-routes [env]
  (routes
   (GET "/main.css" []
     (-> (response stylesheet)
         (content-type "text/css")))
   (GET "/main.js" []
     (-> (resource-response "main.js")
         (content-type "text/javascript")))

   (attendees/attendees-routes env)
   (signup-form/signup-form-routes env)
   (events/events-routes env)
   (home/home-routes env)))

(defn middleware [app env]
  (-> app
      wrap-keyword-params
      wrap-params
      wrap-page-flash
      wrap-flash
      (wrap-session {:store (cookie-store {:key (:cookie-secret env)})})))

(defn handler [env]
  (-> (app-routes env)
      (middleware env)))

(defrecord WebServer [port cookie-secret db]
  component/Lifecycle
  (start [this]
    (assoc this
           ::shutdown-fn
           (http-kit/run-server
            (fn [r] ((handler this) r))
            {:port port})))
  (stop [this]
    (if-let [shutdown-fn (::shutdown-fn this)]
      (do (shutdown-fn :timeout 100)
          (dissoc this ::shutdown-fn))
      this)))

(defn make-server [{::keys [port cookie-secret]}]
  (component/using
   (map->WebServer {:port port
                    :cookie-secret cookie-secret})
   [:db]))
