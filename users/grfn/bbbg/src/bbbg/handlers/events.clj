(ns bbbg.handlers.events
  (:require
   [bbbg.db :as db]
   [bbbg.db.event :as db.event]
   [bbbg.event :as event]
   [bbbg.handlers.core :refer [authenticated? page-response]]
   [bbbg.meetup.import :refer [import-attendees!]]
   [bbbg.util.display :refer [format-date]]
   [bbbg.util.time :as t]
   [bbbg.views.flash :as flash]
   [compojure.coercions :refer [as-uuid]]
   [compojure.core :refer [context GET POST]]
   [java-time :refer [local-date]]
   [ring.util.response :refer [not-found redirect]]))

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

(defn- import-attendee-list-form-group []
  [:div.form-group
   [:label "Import Attendee List"
    [:br]
    [:input {:type :file
             :name :attendees}]]])

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
       (str " Attendee" (when-not (= 1 (:num-attendees event)) "s")))]]
   [:div
    [:form {:method :post
            :action (str "/events/" (::event/id event) "/attendees")
            :enctype "multipart/form-data"}
     (import-attendee-list-form-group)
     [:div.form-group
      [:input {:type :submit
               :value "Import"}]]]]])

(defn event-form
  ([] (event-form {}))
  ([event]
   [:form {:method "POST"
           :action "/events"
           :enctype "multipart/form-data"}
    [:div.form-group
     [:label "Date"
      [:input {:type "date"
               :id "date"
               :name "date"
               :value (str (::event/date event))}]]]
    (import-attendee-list-form-group)
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

    (GET "/new" [date]
      (page-response
       {:title "New Event"}
       (event-form {::event/date date})))

    (POST "/" [date attendees]
      (let [event (db.event/create! db {::event/date date})
            message
            (if attendees
              (let [num-attendees
                    (import-attendees! db
                                       (::event/id event)
                                       (:tempfile attendees))]
                (format "Event created with %d attendees"
                        num-attendees))
              "Event created")]
        (-> (str "/signup-forms/" (::event/id event))
            redirect
            (flash/add-flash {:flash/type :success
                              :flash/message message}))))

    (context "/:id" [id :<< as-uuid]
      (GET "/" []
        (if-let [event (db/fetch db
                                 (-> {:select [:event.*]
                                      :from [:event]
                                      :where [:= :event.id id]}
                                     (db.event/with-stats)))]
          (page-response
           (event-page {:event event}))
          (not-found "Event Not Found")))

      (POST "/attendees" [attendees]
        (let [num-imported (import-attendees! db id (:tempfile attendees))]
          (-> (redirect (str "/events/" id))
              (flash/add-flash
               #:flash{:type :success
                       :message (format "Successfully imported %d attendees"
                                        num-imported)})))))))

(comment
  (def db (:db bbbg.core/system))

  (-> (db/list db :event)
      first
      ::event/date
      format-date)
  )
