;;;; Using irccat to send IRC notifications

(in-package :panettone.irc)

(defun get-irccat-config ()
  "Reads the IRCCATHOST and IRCCATPORT environment variables, and returns them
as two values"
  (destructuring-bind (host port)
      (mapcar #'uiop:getenvp '("IRCCATHOST" "IRCCATPORT"))
    (if (and host port)
        (values host (parse-integer port))
        (values "localhost" 4722))))

(defun send-irc-notification (body &key channel)
  "Sends BODY to the IRC channel CHANNEL (starting with #),
if an IRCCat server is configured (using the IRCCATHOST and IRCCATPORT
environment variables).
May signal a condition if sending fails."
  (multiple-value-bind (irchost ircport) (get-irccat-config)
    (when irchost
      (let ((socket (socket-connect irchost ircport)))
        (unwind-protect
             (progn
               (format (socket-stream socket) "~@[~A ~]~A~%" channel body)
               (finish-output (socket-stream socket)))
          (ignore-errors (socket-close socket)))))))
