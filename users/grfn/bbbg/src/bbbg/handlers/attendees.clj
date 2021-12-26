(ns bbbg.handlers.attendees
  (:require
   [bbbg.attendee :as attendee]
   [bbbg.attendee-check :as attendee-check]
   [bbbg.db :as db]
   [bbbg.db.attendee :as db.attendee]
   [bbbg.db.attendee-check :as db.attendee-check]
   [bbbg.db.event :as db.event]
   [bbbg.event :as event]
   [bbbg.handlers.core :refer [page-response wrap-auth-required]]
   [bbbg.user :as user]
   [bbbg.util.display :refer [format-date]]
   [bbbg.views.flash :as flash]
   [cheshire.core :as json]
   [compojure.coercions :refer [as-uuid]]
   [compojure.core :refer [GET POST routes]]
   [honeysql.helpers :refer [merge-where]]
   [ring.util.response :refer [content-type not-found redirect response]])
  (:import
   java.util.UUID))

(defn- attendees-page [{:keys [attendees q edit-notes]}]
  [:div.page
   [:form.search-form {:method :get :action "/attendees"}
    [:input.search-input
     {:type "search"
      :name "q"
      :value q
      :title "Search Attendees"}]
    [:input {:type "submit"
             :value "Search Attendees"}]]
   [:table.attendees
    [:thead
     [:tr
      [:th "Meetup Name"]
      [:th "Discord Name"]
      [:th "Events RSVPd"]
      [:th "Events Attended"]
      [:th "No-Shows"]
      [:th "Last Vaccination Check"]
      [:th "Notes"]]]
    [:tbody
     (for [attendee (sort-by
                     (comp #{edit-notes} ::attendee/id)
                     (comp - compare)
                     attendees)
           :let [id (::attendee/id attendee)]]
       [:tr
        [:td.attendee-name (::attendee/meetup-name attendee)]
        [:td
         [:label.mobile-label "Discord Name: "]
         (or (not-empty (::attendee/discord-name attendee))
             "—")]
        [:td
         [:label.mobile-label "Events RSVPd: "]
         (:events-rsvpd attendee)]
        [:td
         [:label.mobile-label "Events Attended: "]
         (:events-attended attendee)]
        [:td
         [:label.mobile-label "No-shows: "]
         (:no-shows attendee)]
        [:td
         [:label.mobile-label "Last Vaccination Check: "]
         (if-let [last-check (:last-check attendee)]
           (str "✔️ "(-> last-check
                        ::attendee-check/checked-at
                        format-date)
                ", by "
                (get-in last-check [:user ::user/username]))
           (list
            [:span {:title "Not Checked"}
             "❌"]
            " "
            [:a {:href (str "/attendees/" id "/checks/edit")}
             "Edit"] ))]
        (if (= edit-notes id)
          [:td
           [:form.organizer-notes {:method :post
                                   :action (str "/attendees/" id "/notes")}
            [:div.form-group
             [:input {:type :text :name "notes"
                      :value (::attendee/organizer-notes attendee)
                      :autofocus true}]]
            [:div.form-group
             [:input {:type "Submit" :value "Save Notes"}]]]]
          [:td
           [:p
            (::attendee/organizer-notes attendee)]
           [:p
            [:a {:href (str "/attendees?edit-notes=" id)}
             "Edit Notes"]]])])]]])

(defn attendees-routes [{:keys [db]}]
  (routes
   (wrap-auth-required
    (routes
     (GET "/attendees" [q edit-notes]
       (let [attendees (db/list db (cond-> (db.attendee/with-stats)
                                     q (db.attendee/search q)))
             attendees (db.attendee-check/attendees-with-last-checks
                        db
                        attendees)
             edit-notes (some-> edit-notes UUID/fromString)]
         (page-response (attendees-page {:attendees attendees
                                         :q q
                                         :edit-notes edit-notes}))))

     (POST "/attendees/:id/notes" [id :<< as-uuid notes]
       (if (seq (db/update! db
                            :attendee
                            {::attendee/organizer-notes notes}
                            [:= :id id]))
         (-> (redirect "/attendees")
             (flash/add-flash
              #:flash{:type :success
                      :message "Notes updated successfully"}))
         (not-found "Attendee not found")))))

   (GET "/attendees.json" [q event_id attended]
     (let [results
           (db/list
            db
            (cond->
                (if q
                  (db.attendee/search q)
                  {:select [:attendee.*] :from [:attendee]})
                event_id (db.attendee/for-event event_id)
                (some? attended)
                (merge-where
                 (case attended
                   "true" :attended
                   "false" [:or [:= :attended nil] [:not :attended]]))))]
       (-> {:results results}
           json/generate-string
           response
           (content-type "application/json"))))

   (POST "/event_attendees" [event_id attendee_id]
     (if (and (db/exists? db {:select [:id] :from [:event] :where [:= :id event_id]})
              (db/exists? db {:select [:id] :from [:attendee] :where [:= :id attendee_id]}))
       (do
         (db.event/attended! db {::event/id event_id
                                 ::attendee/id attendee_id})
         (-> (redirect (str "/signup-forms/" event_id))
             (assoc-in [:session :test] 1)
             (flash/add-flash
              #:flash{:type :success
                      :message "Thank you for signing in! Enjoy the event."})))
       (response "Something went wrong")))))

(comment
  (def db (:db bbbg.core/system))
  (db/list db :attendee)
  (db/list db
           (->
            (db.attendee/search "gr")
            (db.attendee/for-event #uuid "9f4f3eae-3317-41a7-843c-81bcae52aebf")))
  (honeysql.format/format
   (->
    (db.attendee/search "gr")
    (db.attendee/for-event #uuid "9f4f3eae-3317-41a7-843c-81bcae52aebf")))
  )
