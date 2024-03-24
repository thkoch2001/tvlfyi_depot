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

(defun and-where* (&rest clauses)
  "Combine all non-nil clauses in CLAUSES into a single S-SQL WHERE form"
  (if (null clauses) t
      (reduce (lambda (x y) `(:and ,x ,y)) clauses)))

(defmacro define-build-time-var
    (name value-if-not-in-build &optional (doc nil))
  `(defvar ,name
     (or (when-let ((package (find-package :build)))
           (let ((sym (find-symbol ,(symbol-name name))))
             (when (boundp sym) (symbol-value sym))))
         ,value-if-not-in-build)
     ,doc))

(defun ->dir (dir)
  (if (char-equal (uiop:last-char dir) #\/)
      dir
      (concatenate 'string dir "/")))
