;;; vector.el --- Working with Elisp's Vector data type -*- lexical-binding: t -*-
;; Author: William Carroll <wpcarro@gmail.com>

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

;; TODO: Consider supporting an alias named tuple for vector.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Library
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst vector/enable-tests? t
  "When t, run the tests defined herein.")

;; TODO: Consider labelling variadic functions like `vector/concat*'
;; vs. `vector/concat'.
(defun vector/concat (&rest args)
  "Return a new vector composed of all vectors in `ARGS'."
  (apply #'vconcat args))

;; TODO: Here's a sketch of a protocol macro being consumed.
;; (definstance monoid vector
;;   :empty (lambda () []))

(defun vector/prepend (x xs)
  "Add `X' to the beginning of `XS'."
  (vector/concat `[,x] xs))

(defun vector/append (x xs)
  "Add `X' to the end of `XS'."
  (vector/concat xs `[,x]))

(defun vector/get (i xs)
  "Return the value in `XS' at index, `I'."
  (aref xs i))

(defun vector/set (i v xs)
  "Set index `I' to value `V' in `XS'.
Returns a copy of `XS' with the updates."
  (let ((copy (vconcat [] xs)))
    (aset copy i v)
    copy))

(defun vector/set! (i v xs)
  "Set index `I' to value `V' in `XS'.
This function mutates XS."
  (aset xs i v))

(when vector/enable-tests?
  (let ((xs [1 2 3])
        (ys [1 2 3]))
    (prelude/assert (= 1 (vector/get 0 ys)))
    (vector/set 0 4 ys)
    (prelude/assert (= 1 (vector/get 0 ys)))
    (prelude/assert (= 1 (vector/get 0 xs)))
    (vector/set! 0 4 xs)
    (prelude/assert (= 4 (vector/get 0 xs)))))

;; TODO: Decide between "remove" and "delete" as the appropriate verbs.
;; TODO: Implement this.
;; (defun vector/delete (i xs)
;;   "Remove the element at `I' in `XS'.")

(provide 'vector)
;;; vector.el ends here
