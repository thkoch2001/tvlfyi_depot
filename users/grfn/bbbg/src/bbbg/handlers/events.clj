(ns bbbg.handlers.events
  (:require
   [bbbg.db :as db]
   [bbbg.db.event :as db.event]
   [bbbg.event :as event]
   [bbbg.handlers.core :refer [page-response]]
   [compojure.core :refer [context GET POST]]
   [ring.util.response :refer [redirect]]))

(defn events-index [events]
  [:ul.events-list
   (for [event events]
     [:li (::event/date event)])])

(defn event-form
  ([] (event-form {}))
  ([event]
   [:form {:method "POST" :action "/events"}
    [:div.form-group
     [:label "Date"
      [:input {:type "date"
               :id "date"
               :name "date"
               :value (str (::event/date event))}]]]
    [:div.form-group
     [:input {:type "submit"
              :value "Create Event"}]]]))

(defn events-routes [{:keys [db]}]
  (context "/events" []
    (GET "/" []
      (let [events (db/list db :event)]
        (events-index events)))

    (GET "/new" [date]
      (page-response
       {:title "New Event"}
       (event-form {::event/date date})))

    (POST "/" [date]
      (let [event (db.event/create! db {::event/date date})]
        (-> (str "/signup-forms/" (::event/id event))
            redirect
            (assoc-in [:flash :message] "Event Created"))))))
