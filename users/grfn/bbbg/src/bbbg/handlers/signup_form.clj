(ns bbbg.handlers.signup-form
  (:require
   [bbbg.db :as db]
   [bbbg.db.event :as db.event]
   [bbbg.event :as event]
   [bbbg.handlers.core :refer [page-response authenticated?]]
   [compojure.core :refer [GET context]]
   [java-time :refer [local-date]]
   [ring.util.response :refer [redirect]]))

(defn no-events-page [{:keys [authenticated?]}]
  [:div.no-events
   [:p
    "There are no events for today"]
   (when authenticated?
     [:p
      [:a {:href (str "/events/new?date=" (str (local-date)))} "Create Event"]
      [:a {:href "/events"} "All Events"]])])

(defn signup-page [event]
  [:div.signup-page
   [:form#signup-form
    {:method "POST"
     :action "/event_attendees"
     :disabled "disabled"}
    [:input#event-id {:type "hidden" :name "event_id" :value (::event/id event)}]
    [:input#attendee-id {:type "hidden" :name "attendee_id"}]
    [:input#name-autocomplete
     {:type "search"
      :title "Name"
      :name "name"
      :spellcheck "false"
      :autocorrect "off"
      :autocomplete "off"
      :autocapitalize "off"
      :maxlength "2048"}]
    [:input {:type "submit"
             :value "Sign In"
             :disabled "disabled"}]]])

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
       (page-response (signup-page event))
       (event-not-found)))))
