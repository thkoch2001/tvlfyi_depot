(ns bbbg.web
  (:require
   [bbbg.handlers.attendees :as attendees]
   [bbbg.handlers.events :as events]
   [bbbg.handlers.home :as home]
   [bbbg.handlers.signup-form :as signup-form]
   [bbbg.styles :refer [stylesheet]]
   [clojure.spec.alpha :as s]
   [com.stuartsierra.component :as component]
   [compojure.core :refer [GET routes]]
   [config.core :refer [env]]
   [org.httpkit.server :as http-kit]
   [ring.middleware.flash :refer [wrap-flash]]
   [ring.middleware.keyword-params :refer [wrap-keyword-params]]
   [ring.middleware.params :refer [wrap-params]]
   [ring.util.response :refer [content-type response resource-response]]))

(s/def ::port pos-int?)

(s/def ::config
  (s/keys :req [::port]))

(s/fdef make-server
  :args (s/cat :config ::config))

(defn env->config []
  (s/assert
   ::config
   {::port (:port env 8888)}))

(defn dev-config []
  (s/assert ::config {::port 8888}))

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

(defn middleware [app]
  (-> app
      wrap-keyword-params
      wrap-params
      wrap-flash))

(defn handler [this]
  (middleware
   (app-routes this)))

(defrecord WebServer [port db]
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

(defn make-server [{::keys [port]}]
  (component/using
   (map->WebServer {:port port})
   [:db]))
