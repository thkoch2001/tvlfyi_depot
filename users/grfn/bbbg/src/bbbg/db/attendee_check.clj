(ns bbbg.db.attendee-check
  (:require
   [bbbg.attendee :as attendee]
   [bbbg.attendee-check :as attendee-check]
   [bbbg.db :as db]
   [bbbg.user :as user]
   [bbbg.util.core :as u]))

(defn attendees-with-last-checks
  [db attendees]
  (when (seq attendees)
    (let [ids (map ::attendee/id attendees)
          checks
          (db/list db {:select [:attendee-check.*]
                       :from [:attendee-check]
                       :join [[{:select [:%max.attendee-check.checked-at
                                         :attendee-check.attendee-id]
                                :from [:attendee-check]
                                :group-by [:attendee-check.attendee-id]
                                :where [:in :attendee-check.attendee-id ids]}
                               :last-check]
                              [:=
                               :attendee-check.attendee-id
                               :last-check.attendee-id]]})
          users (if (seq checks)
                  (u/key-by
                   ::user/id
                   (db/list db {:select [:public.user.*]
                                :from [:public.user]
                                :where [:in :id (map ::user/id checks)]}))
                  {})
          checks (map #(assoc % :user (users (::user/id %))) checks)
          attendee-id->check (u/key-by ::attendee/id checks)]
      (map #(assoc % :last-check (attendee-id->check (::attendee/id %)))
           attendees))))

(comment
  (def db (:db bbbg.core/system))

  (attendees-with-last-checks
   db
   (db/list db :attendee)
   )

  (db/insert! db :attendee-check
              {::attendee/id #uuid "58bcd372-ff6e-49df-b280-23d24c5ba0f0"
               ::user/id #uuid "303fb606-5ef0-4682-ad7d-6429c670cd78"
               ::attendee-check/last-dose-at "2021-12-19"})
  )
