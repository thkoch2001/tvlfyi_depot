(ns bbbg.discord
  (:refer-clojure :exclude [get])
  (:require [clj-http.client :as http]
            [clojure.string :as str]
            [bbbg.util.core :as u]))

(def base-uri "https://discord.com/api")

(defn api-uri [path]
  (str base-uri
       (when-not (str/starts-with? path "/") "/")
       path))

(defn get
  ([token path]
   (get token path {}))
  ([token path params]
   (:body
    (http/get (api-uri path)
              (-> params
                  (assoc :accept :json
                         :as :json)
                  (assoc-in [:headers "authorization"]
                            (str "Bearer " (:token token))))))))

(defn me [token]
  (get token "/users/@me"))

(defn guilds [token]
  (get token "/users/@me/guilds"))

(defn guild-member [token guild-id]
  (get token (str "/users/@me/guilds/" guild-id "/member")))

(comment
  (def token {:token (u/pass "bbbg/test-token")})
  (me token)
  (guilds token)
  (guild-member token "841295283564052510")

  (get token "/guilds/841295283564052510/roles")

  )
