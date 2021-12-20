(ns bbbg.handlers.attendee-checks
  (:require
   [bbbg.attendee :as attendee]
   [bbbg.db :as db]
   [bbbg.handlers.core :refer [page-response wrap-auth-required]]
   [bbbg.util.display :refer [format-date]]
   [compojure.coercions :refer [as-uuid]]
   [compojure.core :refer [context GET POST]]
   [ring.util.response :refer [not-found]]
   [bbbg.attendee-check :as attendee-check]
   [bbbg.user :as user]))

(defn- edit-attendee-checks-page [{:keys [existing-check]
                                   attendee-id ::attendee/id}]
  [:div
   (when existing-check
     [:p
      "Already checked on "
      (-> existing-check ::attendee-check/checked-at format-date)
      " by "
      (::user/username existing-check)])
   [:form {:method :post
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
     (POST "/" []))))
