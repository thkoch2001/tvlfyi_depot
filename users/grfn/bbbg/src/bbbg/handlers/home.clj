(ns bbbg.handlers.home
  (:require
   [bbbg.db.user :as db.user]
   [bbbg.discord.auth :as discord.auth]
   [bbbg.handlers.core :refer [page-response authenticated?]]
   [bbbg.user :as user]
   [bbbg.views.flash :as flash]
   [compojure.core :refer [GET POST routes]]
   [ring.util.response :refer [redirect]]
   [bbbg.discord :as discord]))

(defn- home-page []
  [:div.home-page
   [:a {:href "/signup-forms"}
    "Event Signup Form"]])

(defn auth-failure []
  [:div.auth-failure
   [:p
    "Sorry, only users with the Organizers role in discord can sign in"]
   [:p
    [:a {:href "/"} "Go Back"]]])

(defn home-routes [{:keys [db] :as env}]
  (routes
   (GET "/" [] (page-response (home-page)))

   (POST "/auth/sign-out" request
     (if (authenticated? request)
       (-> (redirect "/")
           (update :session dissoc ::user/id)
           (flash/add-flash
            {:flash/message "Successfully Signed Out"
             :flash/type :success}))
       (redirect "/")))

   (GET "/auth/success" request
     (let [token (get-in request [:oauth2/access-tokens :discord])]
       (if (discord.auth/check-discord-auth env token)
         (let [discord-user (discord/me token)
               user (db.user/find-or-create!
                     db
                     #::user{:username (:username discord-user)
                             :discord-user-id (:id discord-user)})]
           (-> (redirect "/")
               (assoc-in [:session ::user/id] (::user/id user))
               (flash/add-flash
                {:flash/message "Successfully Signed In"
                 :flash/type :success})))
         (->
          (page-response (auth-failure))
          (assoc :status 401)))))))
