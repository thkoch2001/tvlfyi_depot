(ns bbbg.handlers.attendees
  (:require
   [bbbg.attendee :as attendee]
   [bbbg.db :as db]
   [bbbg.db.attendee :as db.attendee]
   [bbbg.db.event :as db.event]
   [bbbg.event :as event]
   [cheshire.core :as json]
   [compojure.core :refer [GET POST routes]]
   [honeysql.helpers :refer [merge-where]]
   [ring.util.response :refer [content-type redirect response]]))

(defn attendees-routes [{:keys [db]}]
  (routes
   (GET "/attendees.json" [q event_id attended]
     (let [results
           (db/list
            db
            (cond->
                (if q
                  (db.attendee/search q)
                  {:select [:attendee.*] :from [:attendee]})
              event_id (db.attendee/for-event event_id)
              (some? attended) (merge-where [:= :attended (case attended
                                                            "true" true
                                                            "false" false)])))]
       (-> {:results results}
           json/generate-string
           response
           (content-type "application/json"))))

   (POST "/event_attendees" [event_id attendee_id]
     (if (and (db/exists? db {:select [:id] :from [:event] :where [:= :id event_id]})
              (db/exists? db {:select [:id] :from [:attendee] :where [:= :id attendee_id]}))
       (do
         (db.event/attended! db {::event/id event_id
                                 ::attendee/id attendee_id})
         (-> (redirect (str "/signup-forms/" event_id))
             (assoc :flash "Thank you for signing in! Enjoy the event.")))
       (response "Something went wrong")))))
