(ns bbbg.db.event
  (:require
   [bbbg.attendee :as attendee]
   [bbbg.db :as db]
   [bbbg.event :as event]
   [honeysql.helpers :refer [merge-group-by merge-left-join merge-select]]
   [java-time :refer [local-date]]))

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

(defn today
  ([] (on-day (local-date)))
  ([db] (db/list db (today))))

(defn with-attendee-counts
  [query]
  (-> query
      (merge-left-join :event_attendee [:= :event.id :event_attendee.event-id])
      (merge-select :%count.event_attendee.attendee_id)
      (merge-group-by :event.id :event_attendee.event-id)))

(comment
  (def db (:db bbbg.core/system))
  (db/list db (-> (today) (with-attendee-counts)))

  (honeysql.format/format
   (honeysql-postgres.helpers/upsert {:insert-into :foo
                                      :values {:bar 1}}
                                     (-> (honeysql-postgres.helpers/on-conflict :did)
                                         (honeysql-postgres.helpers/do-update-set! [:did true]))))
  )
