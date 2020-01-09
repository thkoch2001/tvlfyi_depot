;;; cycle.el --- Simple module for working with cycles. -*- lexical-binding: t -*-
;; Author: William Carroll <wpcarro@gmail.com>

;;; Commentary:
;; Something like this may already exist, but I'm having trouble finding it, and
;; I think writing my own is a nice exercise for learning more Elisp.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dependencies
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'prelude)
(require 'math)
(require 'maybe)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Wish list
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; - TODO: Provide immutable variant.
;; - TODO: Replace mutable consumption with immutable variant.
;; - TODO: Replace indexing with (math/mod current cycle).

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Library
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; `current-index' tracks the current index
;; `xs' is the original list
(cl-defstruct cycle current-index previous-index xs)

(defconst cycle/enable-tests? t
  "When t, run the tests defined herein.")

(defun cycle/new (&rest xs)
  "Create an empty cycle."
  (make-cycle :current-index 0
              :previous-index nil
              :xs xs))

(defun cycle/from-list (xs)
  "Create a cycle from a list of `XS'."
  (make-cycle :current-index 0
              :previous-index nil
              :xs xs))

(defun cycle/to-list (xs)
  "Return the list representation of a cycle, XS."
  (cycle-xs xs))

(defun next-index<- (lo hi x)
  "Return the next index in a cycle when moving downwards.
- `LO' is the lower bound.
- `HI' is the upper bound.
- `X' is the current index."
  (if (< (- x 1) lo)
      (- hi 1)
    (- x 1)))

(defun next-index-> (lo hi x)
  "Return the next index in a cycle when moving upwards.
- `LO' is the lower bound.
- `HI' is the upper bound.
- `X' is the current index."
  (if (>= (+ 1 x) hi)
      lo
    (+ 1 x)))

(defun cycle/previous-focus (cycle)
  "Return the previously focused entry in CYCLE."
  (let ((i (cycle-previous-index cycle)))
    (if (maybe/some? i)
        (nth i (cycle-xs cycle))
      nil)))

(defun cycle/next (xs)
  "Return the next value in `XS' and update `current-index'."
  (let* ((current-index (cycle-current-index xs))
         (next-index (next-index-> 0 (cycle/count xs) current-index)))
    (struct/set! cycle previous-index current-index xs)
    (struct/set! cycle current-index next-index xs)
    (nth next-index (cycle-xs xs))))

(defun cycle/prev (xs)
  "Return the previous value in `XS' and update `current-index'."
  (let* ((current-index (cycle-current-index xs))
         (next-index (next-index<- 0 (cycle/count xs) current-index)))
    (struct/set! cycle previous-index current-index xs)
    (struct/set! cycle current-index next-index xs)
    (nth next-index (cycle-xs xs))))

(defun cycle/current (cycle)
  "Return the current value in `CYCLE'."
  (nth (cycle-current-index cycle) (cycle-xs cycle)))

(defun cycle/count (cycle)
  "Return the length of `xs' in `CYCLE'."
  (length (cycle-xs cycle)))

(defun cycle/jump (i xs)
  "Jump to the I index of XS."
  (let ((current-index (cycle-current-index xs))
        (next-index (math/mod i (cycle/count xs))))
    (struct/set! cycle previous-index current-index xs)
    (struct/set! cycle current-index next-index xs))
  xs)

(defun cycle/focus (p cycle)
  "Focus the element in CYCLE for which predicate, P, is t."
  (let ((i (->> cycle
                cycle-xs
                (-find-index p))))
    (if i
        (cycle/jump i cycle)
      (error "No element in cycle matches predicate"))))

(defun cycle/contains? (x xs)
  "Return t if cycle, XS, has member X."
  (->> xs
       cycle-xs
       (list/contains? x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when cycle/enable-tests?
  (let ((xs (cycle/new 1 2 3)))
    (prelude/assert (maybe/nil? (cycle/previous-focus xs)))
    (prelude/assert (= 1 (cycle/current xs)))
    (prelude/assert (= 2 (cycle/next xs)))
    (prelude/assert (= 1 (cycle/previous-focus xs)))
    (prelude/assert (= 1 (->> xs (cycle/jump 0) cycle/current)))
    (prelude/assert (= 2 (->> xs (cycle/jump 1) cycle/current)))
    (prelude/assert (= 3 (->> xs (cycle/jump 2) cycle/current)))
    (prelude/assert (= 2 (cycle/previous-focus xs)))))

(provide 'cycle)
;;; cycle.el ends here
