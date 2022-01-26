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

;; TODO(sterni): make this a “parser” instead of a predicate
(defun apple-note-p (msg)
  "Checks X-Uniform-Type-Identifier of a MIME:MIME-MESSAGE
  to determine if a given mime message is an Apple Note."
  (when-let (uniform-id (assoc "X-Uniform-Type-Identifier"
                               (mime:mime-message-headers msg)
                               :test #'string-equal))
    (string-equal (cdr uniform-id) "com.apple.mail-note")))

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
      ((string-equal (mime:mime-subtype text) "plain")
       (html-escape-stream (mime:mime-body-stream text :binary nil) out))
      ;; Notes.app creates text/html parts
      ((string-equal (mime:mime-subtype text) "html")
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
