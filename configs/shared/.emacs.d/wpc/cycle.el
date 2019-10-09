;;; cycle.el --- Simple module for working with cycles. -*- lexical-binding: t -*-
;; Author: William Carroll <wpcarro@gmail.com>

;;; Commentary:
;; Something like this may already exist, but I'm having trouble finding it, and
;; I think writing my own is a nice exercise for learning more Elisp.

;;; Code:

(require 'struct)
(require 'math)

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
(cl-defstruct cycle current-index xs)

(defun cycle/new ()
  "Create an empty cycle."
  (make-cycle :current-index 0
              :xs '()))

(defun cycle/from-list (xs)
  "Create a cycle from a list of `XS'."
  (make-cycle :current-index 0
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

(defun cycle/prev (cycle)
  "Return the previous value in `CYCLE' and update `current-index'."
  (let* ((current-index (cycle-current-index cycle))
         (next-index (next-index<- 0 (cycle/count cycle) current-index)))
    (setf (cycle-current-index cycle) next-index)
    (nth next-index (cycle-xs cycle))))

(defun cycle/next (cycle)
  "Return the next value in `CYCLE' and update `current-index'."
  (let* ((current-index (cycle-current-index cycle))
         (next-index (next-index-> 0 (cycle/count cycle) current-index)))
    (setf (cycle-current-index cycle) next-index)
    (nth next-index (cycle-xs cycle))))

(defun cycle/current (cycle)
  "Return the current value in `CYCLE'."
  (nth (cycle-current-index cycle) (cycle-xs cycle)))

(defun cycle/count (cycle)
  "Return the length of `xs' in `CYCLE'."
  (length (cycle-xs cycle)))

(defun cycle/jump (i cycle)
  "Jump to the I index of CYCLE."
  (setf (cycle-current-index cycle)
        (math/mod i (cycle/count cycle)))
  cycle)

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

(provide 'cycle)
;;; cycle.el ends here
