;;; vector.el --- Working with Elisp's Vector data type -*- lexical-binding: t -*-

;; Author: William Carroll <wpcarro@gmail.com>
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1"))

;;; Commentary:
;; It might be best to think of Elisp vectors as tuples in languages like
;; Haskell or Erlang.
;;
;; Not surprisingly, this API is modelled after Elixir's Tuple API.
;;
;; Some Elisp trivia:
;; - "Array": Usually means vector or string.
;; - "Sequence": Usually means list or "array" (see above).
;;
;; It might be a good idea to think of Array and Sequence as typeclasses in
;; Elisp.  This is perhaps more similar to Elixir's notion of the Enum protocol.
;;
;; Intentionally not supporting a to-list function, because tuples can contain
;; heterogenous types whereas lists should contain homogenous types.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Library
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun vector-concat (&rest args)
  "Return a new vector composed of all vectors in `ARGS'."
  (apply #'vconcat args))

(defun vector-prepend (x xs)
  "Add `X' to the beginning of `XS'."
  (vector-concat `[,x] xs))

(defun vector-append (x xs)
  "Add `X' to the end of `XS'."
  (vector-concat xs `[,x]))

(defun vector-get (i xs)
  "Return the value in `XS' at index, `I'."
  (aref xs i))

(defun vector-set (i v xs)
  "Set index `I' to value `V' in `XS'.
Returns a copy of `XS' with the updates."
  (let ((copy (vconcat [] xs)))
    (aset copy i v)
    copy))

(defun vector-set! (i v xs)
  "Set index `I' to value `V' in `XS'.
This function mutates XS."
  (aset xs i v))

(provide 'vector)
;;; vector.el ends here
