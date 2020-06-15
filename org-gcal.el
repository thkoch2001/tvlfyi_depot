;;; ~/.doom.d/org-gcal.el -*- lexical-binding: t; -*-

(require 'aio)
(require 'parse-time)

(setq-local lexical-binding t)
(setq plstore-cache-passphrase-for-symmetric-encryption t)

(defvar gcal-client-id)
(defvar gcal-client-secret)

(defvar google-calendar-readonly-scope
  "https://www.googleapis.com/auth/calendar.readonly")

(defvar events-file "/home/grfn/notes/events.org")

(defun google--get-token (scope client-id client-secret)
  (oauth2-auth-and-store
   "https://accounts.google.com/o/oauth2/v2/auth"
   "https://oauth2.googleapis.com/token"
   scope
   client-id
   client-secret))

(cl-defun google--request (url &key method params scope)
  (let ((p (aio-promise))
        (auth-token (google--get-token scope gcal-client-id gcal-client-secret)))
    (oauth2-url-retrieve
     auth-token
     url
     (lambda (&rest _)
       (goto-char (point-min))
       (re-search-forward "^$")
       (let ((resp (json-parse-buffer :object-type 'alist)))
         (aio-resolve p (lambda () resp))))
     nil
     (or method "GET")
     params)
    p))

(cl-defun list-events (&key min-time max-time)
  (google--request
   (concat
    "https://www.googleapis.com/calendar/v3/calendars/griffin@urbint.com/events"
    "?timeMin=" (format-time-string "%Y-%m-%dT%T%z" min-time)
    "&timeMax=" (format-time-string "%Y-%m-%dT%T%z" max-time))
   :scope google-calendar-readonly-scope))


(defun last-week-events ()
  (list-events :min-time (time-subtract
                          (current-time)
                          (seconds-to-time
                           (* 60 60 24 7)))
               :max-time (current-time)))

(defun next-week-events ()
  (list-events :min-time (current-time)
               :max-time (time-add
                          (current-time)
                          (seconds-to-time
                           (* 60 60 24 7)))))

(defun attending-event? (event)
  (let* ((attendees (append (alist-get 'attendees event) nil))
         (self (--find (alist-get 'self it) attendees)))
    (equal "accepted" (alist-get 'responseStatus self))))

(defun event->org-headline (event level)
  (cl-flet ((make-time
                (key)
                (when-let ((raw-time (->> event (alist-get key) (alist-get 'dateTime))))
                  (format-time-string
                   (org-time-stamp-format t)
                   (parse-iso8601-time-string raw-time)))))
       (if-let ((start-time (make-time 'start))
                (end-time (make-time 'end)))
           (s-format
            "${headline} [[${htmlLink}][${summary}]] :event:
${startTime}--${endTime}
:PROPERTIES:
:LOCATION: ${location}
:EVENT: ${htmlLink}
:END:

${description}"
            (function
             (lambda (k m)
               (or (alist-get (intern k) m)
                   (format "key not found: %s" k))))
            (append
             event
             `((headline . ,(make-string level ?*))
               (startTime . ,start-time)
               (endTime . ,end-time))))
         "")))

(defun write-events (events)
  (with-current-buffer (find-file-noselect events-file)
    (save-mark-and-excursion
      (save-restriction
        (widen)
        (erase-buffer)
        (goto-char (point-min))
        (insert "#+TITLE: Events")
        (newline) (newline)
        (prog1
            (loop for event in (append events nil)
                  when (attending-event? event)
                  do
                  (insert (event->org-headline event 1))
                  (newline)
                  sum 1)
          (org-align-tags t))))))

(defun +grfn/sync-events ()
  (interactive)
  (let* ((events (alist-get 'items (aio-wait-for (next-week-events))))
         (num-written (write-events events)))
    (message "Successfully wrote %d events" num-written)))

(comment
 ((kind . "calendar#event")
  (etag . "\"3174776941020000\"")
  (id . "SNIP")
  (status . "confirmed")
  (htmlLink . "https://www.google.com/calendar/event?eid=SNIP")
  (created . "2020-04-01T13:30:09.000Z")
  (updated . "2020-04-20T13:14:30.510Z")
  (summary . "SNIP")
  (description . "SNIP")
  (location . "SNIP")
  (creator
   (email . "griffin@urbint.com")
   (self . t))
  (organizer
   (email . "griffin@urbint.com")
   (self . t))
  (start
   (dateTime . "2020-04-01T12:00:00-04:00")
   (timeZone . "America/New_York"))
  (end
   (dateTime . "2020-04-01T12:30:00-04:00")
   (timeZone . "America/New_York"))
  (recurrence .
              ["RRULE:FREQ=WEEKLY;UNTIL=20200408T035959Z;BYDAY=WE"])
  (iCalUID . "SNIP")
  (sequence . 0)
  (attendees .
             [((email . "griffin@urbint.com")
               (organizer . t)
               (self . t)
               (responseStatus . "accepted"))
              ((email . "SNIP")
               (displayName . "SNIP")
               (responseStatus . "needsAction"))])
  (extendedProperties
   (private
    (origRecurringId . "309q48kc1dihsvbi13pnlimb5a"))
   (shared
    (origRecurringId . "309q48kc1dihsvbi13pnlimb5a")))
  (reminders
   (useDefault . t)))

 (require 'icalendar)

 (icalendar--convert-recurring-to-diary
  nil
  "RRULE:FREQ=WEEKLY;UNTIL=20200408T035959Z;BYDAY=WE"
  )

 )
