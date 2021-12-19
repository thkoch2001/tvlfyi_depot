(ns bbbg.db.attendee
  (:require
   [bbbg.attendee :as attendee]
   [bbbg.db :as db]
   [bbbg.util.sql :refer [count-where]]
   honeysql-postgres.helpers
   [honeysql.helpers
    :refer
    [merge-group-by merge-join merge-left-join merge-select merge-where]]))

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

(defn with-stats
  ([] (with-stats {:select [:attendee.*]
                   :from [:attendee]}))
  ([query]
   (-> query
       (merge-left-join :event_attendee [:= :attendee.id :event_attendee.attendee_id])
       (merge-group-by :attendee.id)
       (merge-select
        [(count-where :event_attendee.rsvpd_attending) :events-rsvpd]
        [(count-where :event_attendee.attended) :events-attended]
        [(count-where [:and
                       :event_attendee.rsvpd_attending
                       [:not :event_attendee.attended]])
         :no-shows]))))

(comment
  (def db (:db bbbg.core/system))
  (search db "gri")
  (db/insert! db :attendee {::attendee/meetup-name "Griffin Smith"
                            ::attendee/discord-name "grfn"
                            })

  (db/list db (with-stats))
  )
