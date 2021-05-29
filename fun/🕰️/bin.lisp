(defpackage 🕰️.bin
  (:shadow :describe)
  (:use :cl :opts)
  (:import-from :uiop :quit)
  (:import-from :local-time
   :now :timestamp-subtimezone :+utc-zone+
   :*default-timezone* :define-timezone)
  (:import-from :klatre :format-dottime-offset)
  (:import-from :🕰️ :⌚)
  (:export :🚂))

(in-package :🕰️.bin)
(declaim (optimize (safety 3)))

(opts:define-opts
  (:name :help
   :description "Print this help text"
   :short #\h
   :long "help")
  (:name :dot-time
   :description "Use pseudo dot-time format (implies -u)"
   :short #\d
   :long "dot-time")
  (:name :utc
   :description "Display time in UTC instead of local time"
   :short #\u
   :long "utc")
  (:name :no-newline
   :description "Don't print a trailing newline"
   :short #\n
   :long "no-newline"))

(defun make-slash-terminated (str)
  (if (eq (char str (1- (length str))) #\/)
    str
    (concatenate 'string str "/")))

; TODO(sterni): upstream this into local-time
(defun setup-default-timezone ()
  (let* ((tz (uiop:getenv "TZ"))
         (tz-dir (uiop:getenv "TZDIR"))
         (tz-file (if (and tz tz-dir)
                    (merge-pathnames
                      (pathname tz)
                      (pathname (make-slash-terminated tz-dir)))
                    (pathname "/etc/localtime"))))
    (handler-case
      (define-timezone *default-timezone* tz-file :load t)
      (t () (setf *default-timezone* +utc-zone+)))))


(defun 🚂 ()
  (let ((ts (now)))
    (multiple-value-bind (options free-args)
      (handler-case (opts:get-opts)
        ; only handle subset of conditions that can happen here
        (opts:unknown-option (c)
          (format t "error: unknown option ~s~%" (opts:option c))
          (quit 100)))

      ; check if we have any free args we don't know what to do with
      (when (> (length free-args) 0)
        (write-string "error: unexpected command line argument(s): ")
        (loop for arg in free-args
              do (progn (write-string arg) (write-char #\space)))
        (write-char #\newline)
        (quit 100))

      ; print help and exit
      (when (getf options :help)
        (opts:describe :usage-of "🕰️")
        (quit 0))

      ; reinit *default-timezone* as it is cached from compilation
      (setup-default-timezone)
      ; dot-time implies UTC, naturally
      (when (getf options :dot-time)
        (setf (getf options :utc) t))
      ; print clock face
      (format t "~A" (⌚ ts (if (getf options :utc)
                               local-time:+utc-zone+
                               local-time:*default-timezone*)))
      ; render dot-time offset if necessary
      (when (getf options :dot-time)
        (multiple-value-bind (offset-secs _dst _name)
          (timestamp-subtimezone ts local-time:*default-timezone*)
          (write-string
            (format-dottime-offset (round (/ offset-secs 3600))))))
      ; write newline if necessary
      (when (not (getf options :no-newline))
        (write-char #\newline)))))
