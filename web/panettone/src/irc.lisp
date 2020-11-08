;;;; Using irccat to send IRC notifications

(in-package :panettone.irc)

(defun get-irccat-config ()
  "Reads the IRCCATHOST and IRCCATPORT environment variables, and returns them as two values if they both exist (otherwise, returns NIL)."
  (destructuring-bind (host port)
      (mapcar #'uiop:getenvp '("IRCCATHOST" "IRCCATPORT"))
    (when (and host port)
      (values host (parse-integer port)))))

(defun send-irc-notification (body &key channel)
  "Sends BODY to the IRC channel CHANNEL (starting with #), if an IRCCat server is configured (using the IRCCATHOST and IRCCATPORT environment variables)
If CHANNEL is NIL, sends the BODY to the first channel configured in the IRCCat configuration.
May signal a condition if sending fails."
  (multiple-value-bind (irchost ircport)
      (get-irccat-config)
    (when irchost
      (let ((socket (socket-connect irchost ircport)))
        (unwind-protect
             (progn
               (format (socket-stream socket) "~@[~A ~]~A~%" channel body)
               (finish-output (socket-stream socket)))
          (ignore-errors (socket-close socket)))))))
