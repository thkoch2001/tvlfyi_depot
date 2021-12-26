(ns bbbg.handlers.signup-form
  (:require
   [bbbg.attendee :as attendee]
   [bbbg.db :as db]
   [bbbg.db.attendee :as db.attendee]
   [bbbg.db.event :as db.event]
   [bbbg.event :as event]
   [bbbg.handlers.core
    :refer [*authenticated?* authenticated? page-response]]
   [cheshire.core :as json]
   [compojure.core :refer [context GET]]
   [honeysql.helpers :refer [merge-where]]
   [java-time :refer [local-date]]
   [ring.util.response :refer [redirect]]))

(defn no-events-page [{:keys [authenticated?]}]
  [:div.page
   [:p
    "There are no events for today"]
   (when authenticated?
     [:p
      [:a.button {:href (str "/events/new?date=" (str (local-date)))}
       "Create New Event"]])])

(defn signup-page [{:keys [event attendees]}]
  [:div.signup-page
   [:form#signup-form
    {:method "POST"
     :action "/event_attendees"
     :disabled "disabled"}
    [:input#name-autocomplete
     {:type "search"
      :title "Name"
      :name "name"
      :spellcheck "false"
      :autocorrect "off"
      :autocomplete "off"
      :autocapitalize "off"
      :maxlength "2048"}]
    [:input#attendee-id {:type "hidden" :name "attendee_id"}]
    [:input#event-id {:type "hidden" :name "event_id" :value (::event/id event)}]
    [:input#submit-button.hidden
     {:type "submit"
      :value "Sign In"
      :disabled "disabled"}]]
   [:ul#attendees-list
    (if (seq attendees)
      (for [attendee attendees]
        [:li {:data-attendee (json/generate-string attendee)
              :role "button"}
         (::attendee/meetup-name attendee)])
      [:li.no-attendees
       [:p
        "Nobody has RSVPed to this event yet, or no attendee list has been
         imported"]
       (when *authenticated?*
         [:p
          [:a.button
           {:href (str "/events/"
                       (::event/id event)
                       "/attendees/import")}
           "Import Attendee List"]])])]])

(defn event-not-found []
  [:div.event-not-found
   [:p "Event not found"]
   [:p [:a {:href (str "/events/new")} "Create a new event"]]])

;;;

(defn signup-form-routes [{:keys [db]}]
  (context "/signup-forms" []
   (GET "/" request
     (if-let [event (db/fetch db (db.event/today))]
       (redirect (str "/signup-forms/" (::event/id event)))
       (page-response (no-events-page
                       {:authenticated? (authenticated? request)}))))

   (GET "/:event-id" [event-id]
     (if-let [event (db/get db :event event-id)]
       (let [attendees (db/list db
                                (->
                                 (db.attendee/for-event event-id)
                                 (merge-where [:or
                                               [:= :attended nil]
                                               [:not :attended]])))]
         (page-response
          (signup-page {:event event
                        :attendees attendees})))
       (event-not-found)))))
