(ns bbbg.core
  (:gen-class)
  (:require
   [bbbg.db :as db]
   [bbbg.web :as web]
   [clojure.spec.alpha :as s]
   [clojure.spec.test.alpha :as stest]
   [com.stuartsierra.component :as component]
   [expound.alpha :as exp]))

(s/def ::config
  (s/merge
   ::db/config
   ::web/config))

(defn make-system [config]
  (component/system-map
   :db (db/make-database config)
   :web (web/make-server config)))

(defn env->config []
  (s/assert
   ::config
   (merge
    (db/env->config)
    (web/env->config))))

(defn dev-config []
  (s/assert
   ::config
   (merge
    (db/dev-config)
    (web/dev-config))))

(defonce system nil)

(defn init-dev []
  (s/check-asserts true)
  (set! s/*explain-out* exp/printer)
  (stest/instrument))

(defn run-dev []
  (init-dev)
  (alter-var-root
   #'system
   (fn [sys]
     (when sys
       (component/start sys))
     (component/start (make-system (dev-config))))))

(defn -main [& _args]
  (alter-var-root
   #'system
   (constantly (component/start (make-system (env->config))))))

(comment
  (run-dev)
  )
