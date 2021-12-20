(ns bbbg.handlers.events
  (:require
   [bbbg.db :as db]
   [bbbg.db.event :as db.event]
   [bbbg.event :as event]
   [bbbg.handlers.core :refer [authenticated? page-response]]
   [bbbg.util.display :refer [format-date]]
   [bbbg.views.flash :as flash]
   [compojure.coercions :refer [as-uuid]]
   [compojure.core :refer [context GET POST]]
   [java-time :refer [local-date]]
   [ring.util.response :refer [not-found redirect]]
   [bbbg.util.time :as t]))

(defn events-index [{:keys [events authenticated?]}]
  [:div
   (when authenticated?
     [:a {:href "/events/new"}
      "Create New Event"])
   [:ul.events-list
    (for [event events]
      [:li
       [:a {:href (str "/events/" (::event/id event))}
        (format-date (::event/date event))]])]])

(defn event-page [{:keys [event]}]
  [:div.event-page
   [:h1 (format-date (::event/date event))]
   [:div.stats
    [:p (:num-rsvps event) " RSVP"
     (when-not (= 1 (:num-rsvps event)) "s")]
    [:p (:num-attendees event)
     (if (= (t/->LocalDate (::event/date event))
            (local-date))
       " Signed In"
       (str " Attendee" (when-not (= 1 (:num-attendees event)) "s")))]
    ]])

(defn event-form
  ([] (event-form {}))
  ([event]
   [:form {:method "POST" :action "/events"}
    [:div.form-group
     [:label "Date"
      [:input {:type "date"
               :id "date"
               :name "date"
               :value (str (::event/date event))}]]]
    [:div.form-group
     [:input {:type "submit"
              :value "Create Event"}]]]))

(defn events-routes [{:keys [db]}]
  (context "/events" []
    (GET "/" request
      (let [events (db/list db :event)]
        (page-response
         (events-index {:events events
                        :authenticated? (authenticated? request)}))))

    (GET "/:id" [id :<< as-uuid]
      (if-let [event (db/fetch db
                               (-> {:select [:event.*]
                                    :from [:event]
                                    :where [:= :event.id id]}
                                   (db.event/with-stats)))]
        (page-response
         (event-page {:event event}))
        (not-found "Event Not Found")))

    (GET "/new" [date]
      (page-response
       {:title "New Event"}
       (event-form {::event/date date})))

    (POST "/" [date]
      (let [event (db.event/create! db {::event/date date})]
        (-> (str "/signup-forms/" (::event/id event))
            redirect
            (flash/add-flash {:flash/type :success
                              :flash/message "Event Created"}))))))

(comment
  (def db (:db bbbg.core/system))

  (-> (db/list db :event)
      first
      ::event/date
      format-date)
  )
