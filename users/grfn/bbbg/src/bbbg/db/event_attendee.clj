(ns bbbg.db.event-attendee
  (:require honeysql-postgres.format
            [bbbg.db :as db]
            [bbbg.util.core :as u]))

(defn upsert-all!
  [db attendees]
  (db/execute!
   db
   {:insert-into :event-attendee
    :values (map #(->> %
                       (db/process-key-map :event-attendee)
                       (u/map-keys keyword))
                 attendees)
    :upsert {:on-conflict [:event-id :attendee-id]
             :do-update-set [:rsvpd-attending]}}))
