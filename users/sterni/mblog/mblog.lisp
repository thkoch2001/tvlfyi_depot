(in-package :mblog)
(declaim (optimize (safety 3)))

(defun mime-part-mapping (part)
  "Return a CONS cell mapping the Content-Id to the file name of
  a mime attachment or NIL if not a suitable mime message"
  (check-type part mime:mime)
  (handler-case
      (let ((name (assoc :filename (mime:content-disposition-parameters part))))
        (and                                ; a part which can be mapped
         (not (typep part 'mime:text-mime)) ; is not a text message and
         name                               ; has a name
         (cons (mime:content-id part)       ; as well as a Content-Id,
               (cadr name))))
    (error () nil)))                        ; otherwise nil is returned.

(defun make-attachment-map (mime-msg)
  "Build an association list mapping from attachment Content-Ids
  to their respective filenames for a give multipart mime message."
  (check-type mime-msg mime:multipart-mime)
  (loop for part in (mime:content mime-msg)
        for mapping = (mime-part-mapping part)
        when mapping collect mapping))

(defun html-escape (text)
  (closure-html:serialize-lhtml text (closure-html:make-string-sink)))

(defun mime-text-html-fragment (msg attachments)
  (check-type msg mime:text-mime)
  (if (string= (mime:content-subtype msg) "html")
      (apple-note-html-fragment (mime:content msg) attachments)
      (html-escape (mime:content msg))))

(defun note-to-html (mime-msg)
  (cond
    ((typep mime-msg 'mime:text-mime)
     (mime-text-html-fragment mime-msg '()))
    ((typep mime-msg 'mime:multipart-mime)
     (mime-text-html-fragment
      (find-if (lambda (x)
                 (typep x 'mime:text-mime))
               (mime:content mime-msg))
      (make-attachment-map mime-msg)))
    (t (error "Note is an unsupported MIME message")))) ; TODO(sterni): condition

(defun mnote-html ()
  (loop for arg in (uiop:command-line-arguments)
        do (with-open-file (in arg :direction :input)
             (let ((mime-msg (mime:parse-mime in)))
               (princ (note-to-html mime-msg))))))
