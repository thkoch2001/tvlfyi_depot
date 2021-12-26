(ns bbbg.handlers.events
  (:require
   [bbbg.db :as db]
   [bbbg.db.event :as db.event]
   [bbbg.event :as event]
   [bbbg.handlers.core :refer [*authenticated?* page-response]]
   [bbbg.meetup.import :refer [import-attendees!]]
   [bbbg.util.display :refer [format-date pluralize]]
   [bbbg.util.time :as t]
   [bbbg.views.flash :as flash]
   [compojure.coercions :refer [as-uuid]]
   [compojure.core :refer [context GET POST]]
   [java-time :refer [local-date]]
   [ring.util.response :refer [not-found redirect]])
  (:import
   java.time.format.FormatStyle))

(defn- num-attendees [event]
  (str
   (:num-attendees event)
   (if (= (t/->LocalDate (::event/date event))
          (local-date))
     " Signed In"
     (str " Attendee" (when-not (= 1 (:num-attendees event)) "s")))))

(def index-type->label
  {:upcoming "Upcoming"
   :past "Past"})
(def other-index-type
  {:upcoming :past
   :past :upcoming})

(defn events-index
  [{:keys [events num-events type]}]
  [:div.page
   [:div.page-header
    [:h1
     (pluralize
      num-events
      (str (index-type->label type) " Event"))]
    [:a {:href (str "/events"
                    (when (= :upcoming type)
                      "/past"))}
     "View "
     (index-type->label (other-index-type type))
     " Events"]]
   (when *authenticated?*
     [:a.button {:href "/events/new"}
      "Create New Event"])
   [:ul.events-list
    (for [event (sort-by
                 ::event/date
                 (comp - compare)
                 events)]
      [:li
       [:p
        [:a {:href (str "/events/" (::event/id event))}
         (format-date (::event/date event)
                      FormatStyle/FULL)]]
       [:p
        (pluralize (:num-rsvps event) "RSVP")
        ", "
        (num-attendees event)]])]])

(defn- import-attendee-list-form-group []
  [:div.form-group
   [:label "Import Attendee List"
    [:br]
    [:input {:type :file
             :name :attendees}]]])

(defn import-attendees-form [event]
  [:form {:method :post
          :action (str "/events/" (::event/id event) "/attendees")
          :enctype "multipart/form-data"}
   (import-attendee-list-form-group)
   [:div.form-group
    [:input {:type :submit
             :value "Import"}]]])

(defn event-page [{:keys [event]}]
  [:div.page
   [:div.page-header
    [:h1 (format-date (::event/date event)
                      FormatStyle/FULL)]
    [:a {:href (str "/signup-forms/" (::event/id event) )}
     "Go to Signup Form"]]
   [:div.stats
    [:p (pluralize (:num-rsvps event) "RSVP")]
    [:p (num-attendees event)]]
   [:div
    (import-attendees-form event)]])

(defn import-attendees-page [{:keys [event]}]
  [:div.page
   [:h1 "Import Attendees for " (format-date (::event/date event))]
   (import-attendees-form event)])

(defn event-form
  ([] (event-form {}))
  ([event]
   [:div.page
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
               :value "Create Event"}]]]]))

(defn- events-list-handler [db query type]
  (let [events (db/list db (db.event/with-stats query))
        num-events (db/count db query)]
    (page-response
     (events-index {:events events
                    :num-events num-events
                    :type type}))))

(defn events-routes [{:keys [db]}]
  (context "/events" []
    (GET "/" []
      (events-list-handler db (db.event/upcoming) :upcoming))

    (GET "/past" []
      (events-list-handler db (db.event/past) :past))

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

      (GET "/attendees/import" []
        (if-let [event (db/get db :event id)]
          (page-response
           (import-attendees-page {:event event}))
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
