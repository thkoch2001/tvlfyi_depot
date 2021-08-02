(in-package :mblog)
(declaim (optimize (safety 3)))

;;; util

(defun html-escape-stream (in out)
  "Escape characters read from stream IN and write them to
  stream OUT escaped using WHO:ESCAPE-CHAR-MINIMAL."
  (loop for char = (read-char in nil nil)
        while char
        do (write-string (who:escape-char-minimal char) out)))

(defun cid-header-value (cid)
  "Takes a Content-ID as present in Apple Notes' <object> tags and properly
  surrounds them with angle brackets for a MIME header"
  (concatenate 'string "<" cid ">"))

;;; main implementation

(defun apple-note-p (msg)
  "Checks X-Uniform-Type-Identifier of a MIME:MIME-MESSAGE
  to determine if a given mime message is an Apple Note."
  (when-let (uniform-id (assoc "X-Uniform-Type-Identifier"
                               (mime:mime-message-headers msg)
                               :test #'string=))
    (string= (cdr uniform-id) "com.apple.mail-note")))

(defun apple-note-html-fragment (msg out)
  "Takes a MIME:MIME-MESSAGE and writes its text content as HTML to
  the OUT stream. The <object> tags are resolved to <img> which
  refer to the respective attachment's filename as a relative path,
  but extraction of the attachments must be done separately. The
  surrounding <html> and <body> tags are stripped and <head>
  discarded completely, so only a fragment which can be included
  in custom templates remains."
  (let ((text (find-mime-text-part msg)))
    (cond
      ;; Sanity checking of the note
      ((not (apple-note-p msg))
       (error "Unsupported or missing X-Uniform-Type-Identifier"))
      ((not text) (error "Malformed Apple Note: no text part"))
      ;; notemap creates text/plain notes we need to handle properly.
      ;; Additionally we *could* check X-Mailer which notemap sets
      ((string= (mime:mime-subtype text) "plain")
       (html-escape-stream (mime:mime-body-stream text :binary nil) out))
      ;; Notes.app creates text/html parts
      ((string= (mime:mime-subtype text) "html")
       (closure-html:parse
        (mime:mime-body-stream text)
        (make-instance
         'apple-note-transformer
         :cid-lookup
         (lambda (cid)
           (when-let* ((part (mime:find-mime-part-by-id msg (cid-header-value cid)))
                       (file (mime:mime-part-file-name part)))
             file))
         :next-handler
         (closure-html:make-character-stream-sink out))))
      (t (error "Malformed Apple Note: unknown mime type")))))

;;; CLI

(defun mnote-html (args)
  (loop for arg in args
        do (apple-note-html-fragment
            (mime:mime-message (pathname arg))
            *standard-output*)))

(defun mblog (args)
  (format t "💤~%"))

(defparameter +synopsis+ '((mnote-html . "mnote-html FILE [FILE [ ... ]]")
                           (mblog      . "mblog")))

(defun main ()
  (let* ((argv (uiop:raw-command-line-arguments)) ; uiop:argv0 doesn't work?!
         (real-prog-name (and argv (car argv)))
         (prog-name (if (ends-with-subseq "mnote-html" real-prog-name)
                        'mnote-html
                        'mblog))
         (args (and argv (cdr argv)))
         (help-p (find-if (lambda (x)
                            (member x '("-h" "--help" "--usage")
                                    :test #'string=))
                          args)))
    (cond
      (help-p (format *error-output* "Usage: ~A~%"
                      (cdr (assoc prog-name +synopsis+))))
      ((eq prog-name 'mnote-html) (mnote-html args))
      (t (mblog args)))))
