(in-package :mblog)
(declaim (optimize (safety 3)))

;;; util

(defun html-escape-stream (in out)
  (loop for char = (read-char in nil nil)
        while char
        do (write-string (who:escape-char-minimal char) out)))

(defmethod find-mime-text-part ((part mime:mime-text))
  part)

(defmethod find-mime-text-part ((part mime:mime-part))
  nil)

(defmethod find-mime-text-part ((msg mime:mime-message))
  (find-mime-text-part (mime:mime-body msg)))

(defmethod find-mime-text-part ((parts mime:mime-multipart))
  (or (call-next-method)
      (find-if #'find-mime-text-part (mime:mime-parts parts))))

;;; main implementation

(defun apple-note-html-fragment (msg out)
  (let ((text (find-mime-text-part msg)))
    (cond
      ((not text) (error "Malformed Apple Note: no text part"))
      ;; notemap creates text/plain notes we need to handle properly
      ;; TODO(sterni): should we check X-Mailer as well?
      ((string= (mime:mime-subtype text) "plain")
       (html-escape-stream (mime:mime-body-stream text :binary nil) out))
      ;; created by Notes.app
      ((string= (mime:mime-subtype text) "html")
       (closure-html:parse
        (mime:mime-body-stream text)
        (make-instance
         'apple-note-transformer
         :cid-lookup
         (lambda (cid)
           (when-let* ((part (mime:find-mime-part-by-id
                              msg
                              (concatenate 'string "<" cid ">")))
                       (file (mime:mime-part-file-name part)))
             file))
         :next-handler
         (closure-html:make-character-stream-sink out)))))))

(defun mnote-html ()
  (loop for arg in (uiop:command-line-arguments)
        do (apple-note-html-fragment
            (mime:mime-message (pathname arg))
                               *standard-output*)))
