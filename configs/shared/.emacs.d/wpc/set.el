;;; set.el --- Working with mathematical sets -*- lexical-binding: t -*-
;; Author: William Carroll <wpcarro@gmail.com>

;;; Commentary:
;; The set data structure is a collection that deduplicates its elements.

;;; Code:

(require 'ht) ;; friendlier API for hash-tables
(require 'dotted)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Wish List
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; - TODO: Support enum protocol for set.
;; - TODO: Prefer a different hash-table library that doesn't rely on mutative
;;   code.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Library
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl-defstruct set xs)

(defun set/from-list (xs)
  "Create a new set from the list XS."
  (make-set :xs (->> xs
                     (list/map #'dotted/new)
                     ht-from-alist)))

(defun set/new (&rest args)
  "Create a new set from ARGS."
  (set/from-list args))

(defun set/to-list (xs)
  "Map set XS into a list."
  (->> xs
       set-xs
       ht-keys))

(defun set/add (x xs)
  "Add X to set XS."
  (struct/update set
                 xs
                 (lambda (table)
                   (let ((table-copy (ht-copy table)))
                     (ht-set table-copy x 10)
                     table-copy))
                 xs))

(defun set/count (xs)
  "Return the number of elements in XS."
  (->> xs
       set-xs
       ht-size))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Predicates
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun set/empty? (xs)
  "Return t if XS has no elements in it."
  (= 0 (set/count xs)))

(defun set/contains? (x xs)
  "Return t if set XS has X."
  (ht-contains? (set-xs xs) x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst set/enable-testing? t
  "Run tests when t.")

(when set/enable-testing?
  (progn
    ;; {from,to}-list
    (prelude/assert (equal '(1 2 3)
                           (->> '(1 1 2 2 3 3)
                                set/from-list
                                set/to-list)))
    ;; empty?
    (prelude/assert (set/empty? (set/new)))
    (prelude/refute (set/empty? (set/new 1 2 3)))
    ;; count
    (prelude/assert (= 0 (set/count (set/new))))
    (prelude/assert (= 2 (set/count (set/new 1 1 2 2))))))

(provide 'set)
;;; set.el ends here
