(ns bbbg.views.flash
  (:require [clojure.spec.alpha :as s]))

(s/def :flash/type #{:success :error :warning})
(s/def :flash/message string?)
(s/def ::flash (s/keys :req [:flash/type :flash/message]))
(s/fdef add-flash :args (s/cat :resp map? :flash ::flash) :ret map?)

;;;

(def ^:dynamic *flash* nil)

(defn wrap-page-flash [handler]
  (fn
    ([request]
     (binding [*flash* (:flash request)]
       (handler request)))
    ([request respond raise]
     (binding [*flash* (:flash request)]
       (handler request respond raise)))))

(defn add-flash [resp flash]
  (update-in resp [:flash :flash/messages] conj flash))

(defn render-flash
  ([] (render-flash *flash*))
  ([flash]
   (when-some [messages (not-empty (:flash/messages flash))]
     [:ul.flash-messages
      (for [message messages]
        [:li.flash-message
         {:class (str "flash-" (-> message :flash/type name))}
         (:flash/message message)])])))
