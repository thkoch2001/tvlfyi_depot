(in-package :note)
(declaim (optimize (safety 3)))

;;; util

;; TODO(sterni): merge this with mblog::*copy-buffer-size*
(defvar *copy-buffer-size* 4096)

(defun html-escape-stream (in out)
  "Escape characters read from stream IN and write them to
  stream OUT escaped using WHO:ESCAPE-STRING-MINIMAL."
  (let ((buf (make-string *copy-buffer-size*)))
    (loop for len = (read-sequence buf in)
          while (> len 0)
          do (write-string (who:escape-string-minimal (subseq buf 0 len)) out))))

(defun cid-header-value (cid)
  "Takes a Content-ID as present in Apple Notes' <object> tags and properly
  surrounds them with angle brackets for a MIME header"
  (concatenate 'string "<" cid ">"))

;; TODO(sterni): move into mime4cl
(defun find-mime-message-header (header-name message)
  (when-let ((header (assoc header-name
                            (mime:mime-message-headers message)
                            :test #'string-equal)))
    (cdr header)))

(defun find-mime-message-date (message)
  (when-let ((date-string (find-mime-message-header "Date" message)))
    (date-time-parser:parse-date-time date-string)))

;;; main implementation

(defun apple-note-mime-subtype-p (x)
  (member x '("plain" "html") :test #'string-equal))

(deftype apple-note-mime-subtype ()
  '(satisfies apple-note-mime-subtype-p))

(defclass apple-note (mime:mime-message)
  ((text-part
    :type mime:mime-text
    :initarg :text-part
    :reader apple-note-text-part)
   (subject
    :type string
    :initarg :subject
    :reader apple-note-subject)
   (uuid
    :type string
    :initarg :uuid
    :reader apple-note-uuid)
   (time
    :type integer
    :initarg :time
    :reader apple-note-time)
   (mime-subtype
    :type apple-note-mime-subtype
    :initarg :mime-subtype
    :reader apple-note-mime-subtype))
  (:documentation
   "Representation of a Note created using Apple's Notes via the IMAP backend"))

(defun apple-note-p (msg)
  "Checks X-Uniform-Type-Identifier of a MIME:MIME-MESSAGE
  to determine if a given mime message claims to be an Apple Note."
  (when-let (uniform-id (assoc "X-Uniform-Type-Identifier"
                               (mime:mime-message-headers msg)
                               :test #'string-equal))
    (string-equal (cdr uniform-id) "com.apple.mail-note")))

(defun decode-RFC2047-to-string (input)
  (apply
   #'concatenate
   (cons 'string
         (mapcar
          (lambda (el)
            (etypecase el
              (cons (babel:octets-to-string
                     (car el)
                     :encoding (babel-encodings:get-character-encoding
                                (intern (string-upcase (cdr el)) 'keyword))))
              (string el)))
          (mime:parse-RFC2047-text input)))))

(defun make-apple-note (msg)
  (check-type msg mime-message)

  (unless (apple-note-p msg)
    (error "Passed message is not an Apple Note according to headers"))

  (let ((text-part (mime:find-mime-text-part msg))
        (subject (when-let ((val (find-mime-message-header "Subject" msg)))
                   ;; TODO(sterni): mime4cl should do this
                   (decode-RFC2047-to-string val)))
        (uuid (when-let ((val (find-mime-message-header
                               "X-Universally-Unique-Identifier"
                               msg)))
                (string-downcase val)))
        (time (find-mime-message-date msg)))
    ;; The idea here is that we don't need to check a lot manually, instead
    ;; the type annotation are going to do this for us (with sufficient safety?)
    (change-class msg 'apple-note
                  :text-part text-part
                  :subject subject
                  :uuid uuid
                  :time time
                  :mime-subtype (mime:mime-subtype text-part))))

(defgeneric apple-note-html-fragment (note out)
  (:documentation
   "Takes an APPLE-NOTE and writes its text content as HTML to
   the OUT stream. The <object> tags are resolved to <img> which
   refer to the respective attachment's filename as a relative path,
   but extraction of the attachments must be done separately. The
   surrounding <html> and <body> tags are stripped and <head>
   discarded completely, so only a fragment which can be included
   in custom templates remains."))

(defmethod apple-note-html-fragment ((note apple-note) (out stream))
  (let ((text (apple-note-text-part note)))
    (cond
      ;; notemap creates text/plain notes we need to handle properly.
      ;; Additionally we *could* check X-Mailer which notemap sets
      ((string-equal (apple-note-mime-subtype note) "plain")
       (html-escape-stream (mime:mime-body-stream text :binary nil) out))
      ;; Notes.app creates text/html parts
      ((string-equal (apple-note-mime-subtype note) "html")
       (closure-html:parse
        (mime:mime-body-stream text)
        (make-instance
         'apple-note-transformer
         :cid-lookup
         (lambda (cid)
           (when-let* ((part (mime:find-mime-part-by-id note (cid-header-value cid)))
                       (file (mime:mime-part-file-name part)))
             file))
         :next-handler
         (closure-html:make-character-stream-sink out))))
      (t (error "Internal error: unexpected MIME subtype")))))
