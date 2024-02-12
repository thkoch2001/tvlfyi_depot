(ns bbbg.db.user
  (:require [bbbg.db :as db]
            [bbbg.user :as user]))

(defn create! [db attrs]
  (db/insert! db
              :public.user
              (select-keys attrs [::user/id
                                  ::user/username
                                  ::user/discord-user-id])))

(defn find-or-create! [db attrs]
  (or
   (db/fetch db {:select [:*]
                 :from [:public.user]
                 :where [:=
                         :discord-user-id
                         (::user/discord-user-id attrs)]})
   (create! db attrs)))
