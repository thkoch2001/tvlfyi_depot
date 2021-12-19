(ns bbbg.db.user
  (:require [bbbg.db :as db]
            [bbbg.user :as user]))

(defn create! [db attrs]
  (db/insert! db
              :public.user
              (select-keys attrs [::user/id
                                  ::user/username
                                  ::user/discord-user-id])))
