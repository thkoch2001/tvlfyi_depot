(ns bbbg.db.attendee
  (:require
   [bbbg.db :as db]
   honeysql-postgres.helpers
   [honeysql.helpers :refer [merge-join merge-where]]))

(defn search
  ([query]
   (cond->
       {:select [:attendee.*]
        :from [:attendee]}
     query
     (assoc
      :where [:or
              [:ilike :meetup_name (str "%" query "%")]
              [:ilike :discord_name (str "%" query "%")]])))
  ([db query]
   (db/list db (search query))))

(defn for-event
  ([query event-id]
   (-> query
       (merge-join :event_attendee [:= :attendee.id :event_attendee.attendee_id])
       (merge-where [:= :event_attendee.event_id event-id]))))

(comment
  (def db (:db bbbg.core/system))
  (search db "gri")
  )
