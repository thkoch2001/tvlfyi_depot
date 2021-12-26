(ns bbbg.handlers.attendee-checks
  (:require
   [bbbg.attendee :as attendee]
   [bbbg.attendee-check :as attendee-check]
   [bbbg.db :as db]
   [bbbg.db.attendee-check :as db.attendee-check]
   [bbbg.handlers.core :refer [page-response wrap-auth-required]]
   [bbbg.user :as user]
   [bbbg.util.display :refer [format-date]]
   [compojure.coercions :refer [as-uuid]]
   [compojure.core :refer [context GET POST]]
   [ring.util.response :refer [not-found redirect]]
   [bbbg.views.flash :as flash]))

(defn- edit-attendee-checks-page [{:keys [existing-check]
                                   attendee-id ::attendee/id}]
  [:div.page
   (when existing-check
     [:p
      "Already checked on "
      (-> existing-check ::attendee-check/checked-at format-date)
      " by "
      (::user/username existing-check)])
   [:form.attendee-checks-form
    {:method :post
     :action (str "/attendees/" attendee-id "/checks")}
    [:div.form-group
     [:label
      "Last Dose"
      [:input {:type :date
               :name :last-dose-at}]]]
    [:div.form-group
     [:input {:type :submit
              :value "Mark Checked"}]]]])

(defn attendee-checks-routes [{:keys [db]}]
  (wrap-auth-required
   (context "/attendees/:attendee-id/checks" [attendee-id :<< as-uuid]
     (GET "/edit" []
       (if (db/exists? db {:select [1]
                           :from [:attendee]
                           :where [:= :id attendee-id]})
         (let [existing-check (db/fetch
                               db
                               {:select [:attendee-check.*
                                         :public.user.*]
                                :from [:attendee-check]
                                :join [:public.user
                                       [:=
                                        :attendee-check.user-id
                                        :public.user.id]]
                                :where [:= :attendee-id attendee-id]})]
           (page-response
            (edit-attendee-checks-page
             {:existing-check existing-check
              ::attendee/id attendee-id})))
         (not-found "Attendee not found")))
     (POST "/" {{:keys [last-dose-at]} :params
                {user-id ::user/id} :session}
       (db.attendee-check/create!
        db
        {::attendee/id attendee-id
         ::user/id user-id
         ::attendee-check/last-dose-at last-dose-at})
       (-> (redirect "/attendees")
           (flash/add-flash
            #:flash{:type :success
                    :message "Successfully updated vaccination status"}))))))
