;;; sequence.el --- Working with the "sequence" types -*- lexical-binding: t -*-
;; Author: William Carroll <wpcarro@gmail.com>

;;; Commentary:
;; Elisp supports a typeclass none as "sequence" which covers the following
;; types:
;; - list: '(1 2 3 4 5)
;; - vector: ["John" 27 :blue]
;; - string: "To be or not to be..."

;; TODO: Document the difference between a "reduce" and a "fold".  I.e. - reduce
;; has an initial value whereas fold uses the first element in the sequence as
;; the initial value.
;;
;; Note: This should be an approximation of Elixir's Enum protocol albeit
;; without streams.
;;
;; Elisp has done a lot of this work already and these are mostly wrapper
;; functions.
;; See the following list for reference:
;; - sequencep
;; - elt
;; - copy-sequence
;; - reverse
;; - nreverse
;; - sort
;; - seq-elt
;; - seq-length
;; - seqp
;; - seq-drop
;; - seq-take
;; - seq-take-while
;; - seq-drop-while
;; - seq-do
;; - seq-map
;; - seq-mapn
;; - seq-filter
;; - seq-remove
;; - seq-reduce
;; - seq-some
;; - seq-find
;; - seq-every-p
;; - seq-empty-p
;; - seq-count
;; - seq-sort
;; - seq-contains
;; - seq-position
;; - seq-uniq
;; - seq-subseq
;; - seq-concatenate
;; - seq-mapcat
;; - seq-partition
;; - seq-intersection
;; - seq-difference
;; - seq-group-by
;; - seq-into
;; - seq-min
;; - seq-max
;; - seq-doseq
;; - seq-let

;;; Code:

;; Perhaps we can provide default implementations for `filter' and `map' derived
;; from the `reduce' implementation.
;; (defprotocol sequence
;;   :functions (reduce))
;; (definstance sequence list
;;   :reduce #'list/reduce
;;   :filter #'list/filter
;;   :map    #'list/map)
;; (definstance sequence vector
;;   :reduce #'vector/reduce)
;; (definstance sequence string
;;   :reduce #'string)

(defun sequence/classify (xs)
  "Return the type of `XS'."
  (cond
   ((listp xs) 'list)
   ((vectorp xs) 'vector)
   ((stringp xs) 'string)))

(defun sequence/reduce (acc f xs)
  "Reduce of `XS' calling `F' on x and `ACC'."
  (seq-reduce
   (lambda (acc x)
     (funcall f x acc))
   xs
   acc))

;; Elixir also turned everything into a list for efficiecy reasons.

(defun sequence/filter (p xs)
  "Filter `XS' with predicate, `P'.
Returns a list regardless of the type of `XS'."
  (seq-filter p xs))

(defun sequence/map (f xs)
  "Maps `XS' calling `F' on each element.
Returns a list regardless of the type of `XS'."
  (seq-map f xs))

(provide 'sequence)
;;; sequence.el ends here
