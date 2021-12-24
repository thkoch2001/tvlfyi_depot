(ns bbbg.meetup.import
  (:require
   [bbbg.attendee :as attendee]
   [bbbg.db.attendee :as db.attendee]
   [bbbg.db.event-attendee :as db.event-attendee]
   [bbbg.event :as event]
   [bbbg.event-attendee :as event-attendee]
   [bbbg.meetup-user :as meetup-user]
   [bbbg.util.core :as u]
   [bbbg.util.spec :as u.s]
   [clojure.data.csv :as csv]
   [clojure.java.io :as io]
   [clojure.spec.alpha :as s]
   [clojure.string :as str]
   [expound.alpha :as exp]))

(def spreadsheet-column->key
  {"Name" :name
   "User ID" :user-id
   "Title" :title
   "Event Host" :event-host
   "RSVP" :rsvp
   "Guests" :guests
   "RSVPed on" :rsvped-on
   "Joined Group on" :joined-group-on
   "URL of Member Profile" :member-profile-url})

(defn read-attendees [f]
  (with-open [reader (io/reader f)]
    (let [[headers & rows] (-> reader (csv/read-csv :separator \tab))
          keys (map spreadsheet-column->key headers)]
      (doall
       (->> rows
            (map (partial zipmap keys))
            (map (partial u/filter-kv (fn [k _] (some? k))))
            (filter (partial some (comp seq val))))))))

;;;

(s/def ::imported-attendee
  (s/keys :req [::attendee/meetup-name
                ::meetup-user/id]))

(def key->attendee-col
  {:name ::attendee/meetup-name
   :user-id ::meetup-user/id})

(defn row-user-id->user-id [row-id]
  (str/replace-first row-id "user " ""))

(defn check-attendee [attendee]
  ()
  (if (s/valid? ::imported-attendee attendee)
    attendee
    (throw (ex-info
            (str "Invalid imported attendee\n"
                 (exp/expound-str ::imported-attendee attendee))
            (assoc (s/explain-data ::imported-attendee attendee)
                   ::s/failure
                   ::s/assertion-failed)))))

(defn row->attendee [r]
  (u.s/assert!
   ::imported-attendee
   (update (u/keep-keys key->attendee-col r)
           ::meetup-user/id row-user-id->user-id)))

;;;

(s/def ::imported-event-attendee
  (s/keys :req [::event-attendee/rsvpd-attending?
                ::attendee/id
                ::event/id]))

(def key->event-attendee-col
  {:rsvp ::event-attendee/rsvpd-attending?})

(defn row->event-attendee
  [{event-id ::event/id :keys [meetup-id->attendee-id]} r]
  (let [attendee-id (-> r :user-id row-user-id->user-id meetup-id->attendee-id)]
    (u.s/assert!
     ::imported-event-attendee
     (-> (u/keep-keys key->event-attendee-col r)
         (update ::event-attendee/rsvpd-attending?
                 (partial = "Yes"))
         (assoc ::event/id event-id
                ::attendee/id attendee-id)))))

;;;

(defn import-data! [db event-id f]
  (let [rows (read-attendees f)
        attendees (db.attendee/upsert-all! db (map row->attendee rows))
        meetup-id->attendee-id (into {}
                                     (map (juxt ::meetup-user/id ::attendee/id))
                                     attendees)]
    (db.event-attendee/upsert-all!
     db
     (map (partial row->event-attendee
                   {::event/id event-id
                    :meetup-id->attendee-id meetup-id->attendee-id})
          rows))
    (count rows)))

;;; Spreadsheet columns:
;;;
;;; Name
;;; User ID
;;; Title
;;; Event Host
;;; RSVP
;;; Guests
;;; RSVPed on
;;; Joined Group on
;;; URL of Member Profile
;;; Have you been to one of our events before? Note, attendance at all events will require proof of vaccination until further notice.

(comment
  (def -filename- "/home/grfn/code/depot/users/grfn/bbbg/sample-data.tsv")
  (def event-id #uuid "09f8fed6-7480-451b-89a2-bb4edaeae657")

  (read-attendees -filename-)
  (import-data! (:db bbbg.core/system) event-id -filename-)

  )
