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

(defun cycle/from-list (xs)
  "Create a cycle from a list of `XS'."
  (if (= 0 (length xs))
      (make-cycle :current-index nil
                  :previous-index nil
                  :xs xs)
    (make-cycle :current-index 0
                :previous-index nil
                :xs xs)))

(defun cycle/new (&rest xs)
  "Create a cycle with XS as the values."
  (cycle/from-list xs))

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

;; TODO: Consider adding "!" to the function name herein since many of them
;; mutate the collection, and the APIs are beginning to confuse me.
(defun cycle/focus-previous! (xs)
  "Jump to the item in XS that was most recently focused; return the cycle.
This will error when previous-index is nil.  This function mutates the
underlying struct."
  (let ((i (cycle-previous-index xs)))
    (if (maybe/some? i)
        (progn
          (cycle/jump i xs)
          (cycle/current xs))
      (error "Cannot focus the previous element since cycle-previous-index is nil"))))

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

(defun cycle/focus-item (x xs)
  "Focus ITEM in cycle XS.
ITEM is the first item in XS that t for `equal'."
  (cycle/focus (lambda (y) (equal x y)) xs))

(defun cycle/contains? (x xs)
  "Return t if cycle, XS, has member X."
  (->> xs
       cycle-xs
       (list/contains? x)))

(defun cycle/empty? (xs)
  "Return t if cycle XS has no elements."
  (= 0 (length (cycle-xs xs))))

(defun cycle/focused? (xs)
  "Return t if cycle XS has a non-nil value for current-index."
  (maybe/some? (cycle-current-index xs)))

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
    (prelude/assert (= 2 (cycle/previous-focus xs)))
    (prelude/assert (= 2 (cycle/focus-previous! xs)))))

(provide 'cycle)
;;; cycle.el ends here
