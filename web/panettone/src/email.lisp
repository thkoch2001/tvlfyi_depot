(in-package :panettone.email)
(declaim (optimize (safety 3)))

(defvar *smtp-server* "localhost"
  "The host for SMTP connections")

(defvar *smtp-server-port* 2525
  "The port for SMTP connections")

(defvar *notification-from* "tvlbot@tazj.in"
  "The email address to send email notifications from")

(defvar *notification-from-display-name* "Panettone"
  "The Display Name to use when sending email notifications")

(defvar *notification-subject-prefix* "[panettone]"
  "String to prefix all email subjects with")

(defun send-email-notification (&key to subject message)
  "Sends an email to TO with the given SUBJECT and MESSAGE, using the current
values of `*smtp-server*', `*smtp-server-port*' and `*email-notification-from*'"
  (let ((subject (if *notification-subject-prefix*
                     (format nil "~A ~A"
                             *notification-subject-prefix*
                             subject)
                     subject)))
    (cl-smtp:send-email
     *smtp-server*
     *notification-from*
     to
     subject
     message
     :port *smtp-server-port*
     :display-name *notification-from-display-name*)))

(defun user-has-email-notifications-enabled-p (dn)
  "Returns T if the user with the given DN has enabled email notifications"
  (enable-email-notifications-p (settings-for-user dn)))

(defun notify-user (dn &key subject message)
  "Sends an email notification to the user with DN with the given SUBJECT and
  MESSAGE, iff that user has not disabled email notifications"
  (when (user-has-email-notifications-enabled-p dn)
    (when-let ((user (find-user-by-dn dn)))
      (send-email-notification
       :to (mail user)
       :subject subject
       :message message))))
