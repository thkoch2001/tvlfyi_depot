(ns bbbg.handlers.signup-form
  (:require
   [bbbg.db :as db]
   [bbbg.db.event :as db.event]
   [bbbg.event :as event]
   [bbbg.handlers.core :refer [page-response authenticated? *authenticated?*]]
   [compojure.core :refer [GET context]]
   [java-time :refer [local-date]]
   [ring.util.response :refer [redirect]]
   [bbbg.db.attendee :as db.attendee]
   [cheshire.core :as json]
   [bbbg.attendee :as attendee]))

(defn no-events-page [{:keys [authenticated?]}]
  [:div.no-events
   [:p
    "There are no events for today"]
   (when authenticated?
     [:p
      [:a {:href (str "/events/new?date=" (str (local-date)))} "Create Event"]
      [:a {:href "/events"} "All Events"]])])

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
       (let [attendees (db.attendee/for-event db event-id)]
         (page-response
          (signup-page {:event event
                        :attendees attendees})))
       (event-not-found)))))
