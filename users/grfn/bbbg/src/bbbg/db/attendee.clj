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
  ([q] (search {:select [:attendee.*] :from [:attendee]} q))
  ([db-or-query q]
   (if (db/database? db-or-query)
     (db/list db-or-query (search q))
     (cond-> db-or-query
       q [:or
          [:ilike :meetup_name (str "%" q "%")]
          [:ilike :discord_name (str "%" q "%")]])))
  ([db query q]
   (db/list db (search query q))))

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
  (db/database? db)
  (search db "gri")
  (db/insert! db :attendee {::attendee/meetup-name "Griffin Smith"
                            ::attendee/discord-name "grfn"
                            })

  (search db (with-stats) "gri")

  (search (with-stats) "gri")

  (db/list db (with-stats))

  (db/insert! db :attendee {::attendee/meetup-name "Rando Guy"
                            ::attendee/discord-name "rando"})
  )
