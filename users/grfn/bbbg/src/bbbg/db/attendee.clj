(ns bbbg.db.attendee
  (:require
   [bbbg.attendee :as attendee]
   [bbbg.db :as db]
   [bbbg.util.sql :refer [count-where]]
   honeysql-postgres.helpers
   [honeysql.helpers
    :refer
    [merge-group-by merge-join merge-left-join merge-select merge-where]]
   [bbbg.util.core :as u]))

(defn search
  ([q] (search {:select [:attendee.*] :from [:attendee]} q))
  ([db-or-query q]
   (if (db/database? db-or-query)
     (db/list db-or-query (search q))
     (cond-> db-or-query
       q (merge-where
          [:or
           [:ilike :meetup_name (str "%" q "%")]
           [:ilike :discord_name (str "%" q "%")]]))))
  ([db query q]
   (db/list db (search query q))))

(defn for-event
  ([db-or-query event-id]
   (if (db/database? db-or-query)
     (for-event db-or-query
                {:select [:attendee.*]
                 :from [:attendee]}
                event-id)
     (-> db-or-query
         (merge-join :event_attendee [:= :attendee.id :event_attendee.attendee_id])
         (merge-where [:= :event_attendee.event_id event-id]))))
  ([db query event-id]
   (db/list db (for-event query event-id))))

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

(defn upsert-all!
  [db attendees]
  (db/list
   db
   {:insert-into :attendee
    :values (map #(->> %
                       (db/process-key-map :attendee)
                       (u/map-keys keyword))
                 attendees)
    :upsert {:on-conflict [:meetup-user-id]
             :do-update-set [:meetup-name]}
    :returning [:id :meetup-user-id]}))

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
