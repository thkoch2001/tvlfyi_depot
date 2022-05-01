(ns bbbg.db.event
  (:require
   [bbbg.attendee :as attendee]
   [bbbg.db :as db]
   [bbbg.event :as event]
   [bbbg.util.sql :refer [count-where]]
   [honeysql.helpers
    :refer [merge-group-by merge-left-join merge-select merge-where]]
   [java-time :refer [local-date local-date-time local-time]]))

(defn create! [db event]
  (db/insert! db :event (select-keys event [::event/date])))

(defn attended!
  [db params]
  (db/execute!
   db
   {:insert-into :event-attendee
    :values [{:event_id (::event/id params)
              :attendee_id (::attendee/id params)
              :attended true}]
    :upsert {:on-conflict [:event-id :attendee-id]
             :do-update-set! {:attended true}}}))

(defn on-day
  ([day] {:select [:event.*]
          :from [:event]
          :where [:= :date (str day)]})
  ([db day]
   (db/list db (on-day day))))


(def end-of-day-hour
  ;; 7am utc = 3am nyc
  7)

(defn current-day
  ([] (current-day (local-date-time)))
  ([dt]
   (if (<= 0
           (.getHour (local-time dt))
           end-of-day-hour)
     (java-time/minus
      (local-date dt)
      (java-time/days 1))
     (local-date dt))))

(comment
  (current-day
   (local-date-time
    2022 5 1
    1 13 0))
  )

(defn today
  ([] (on-day (current-day)))
  ([db] (db/list db (today))))

(defn upcoming
  ([] (upcoming {:select [:event.*] :from [:event]}))
  ([query]
   (merge-where query [:>= :date (local-date)])))

(defn past
  ([] (past {:select [:event.*] :from [:event]}))
  ([query]
   (merge-where query [:< :date (local-date)])))

(defn with-attendee-counts
  [query]
  (-> query
      (merge-left-join :event_attendee [:= :event.id :event_attendee.event-id])
      (merge-select :%count.event_attendee.attendee_id)
      (merge-group-by :event.id :event_attendee.event-id)))

(defn with-stats
  [query]
  (-> query
      (merge-left-join :event_attendee [:= :event.id :event_attendee.event-id])
      (merge-select
       [(count-where :event-attendee.rsvpd_attending) :num-rsvps]
       [(count-where :event-attendee.attended) :num-attendees])
      (merge-group-by :event.id)))

(comment
  (def db (:db bbbg.core/system))
  (db/list db (-> (today) (with-attendee-counts)))

  (honeysql.format/format
   (honeysql-postgres.helpers/upsert {:insert-into :foo
                                      :values {:bar 1}}
                                     (-> (honeysql-postgres.helpers/on-conflict :did)
                                         (honeysql-postgres.helpers/do-update-set! [:did true]))))
  )
