(in-package :panettone.util)

(defun integer-env (var &key default)
  (or
   (when-let ((str (uiop:getenvp var)))
     (try-parse-integer str))
   default))

(defun add-missing-base64-padding (s)
  "Add any missing padding characters to the (un-padded) base64 string `S', such
that it can be successfully decoded by the `BASE64' package"
  ;; I apologize
  (let* ((needed-padding (mod (length s) 4))
         (pad-chars (if (zerop needed-padding) 0 (- 4 needed-padding))))
    (format nil "~A~v@{~A~:*~}" s pad-chars "=")))
