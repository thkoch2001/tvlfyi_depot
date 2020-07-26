(in-package :panettone.util)

(defun integer-env (var &key default)
  (or
   (when-let ((str (uiop:getenvp var)))
     (try-parse-integer str))
   default))
