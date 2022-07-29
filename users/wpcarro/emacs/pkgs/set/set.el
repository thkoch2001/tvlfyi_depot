;;; set.el --- Working with mathematical sets -*- lexical-binding: t -*-

;; Author: William Carroll <wpcarro@gmail.com>
;; Version: 0.0.1
;; Package-Requires: ((emacs "24.3"))

;;; Commentary:
;; The set data structure is a collection that deduplicates its elements.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dependencies
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'cl-lib)
(require 'dash)
(require 'ht) ;; friendlier API for hash-tables
(require 'struct)

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

(defun set-from-list (xs)
  "Create a new set from the list XS."
  (make-set :xs (->> xs
                     (-map (lambda (x) (cons x nil)))
                     ht-from-alist)))

(defun set-new (&rest args)
  "Create a new set from ARGS."
  (set-from-list args))

(defun set-to-list (xs)
  "Map set XS into a list."
  (->> xs
       set-xs
       ht-keys))

(defun set-add (x xs)
  "Add X to set XS."
  (struct-update set
                 xs
                 (lambda (table)
                   (let ((table-copy (ht-copy table)))
                     (ht-set table-copy x nil)
                     table-copy))
                 xs))

;; TODO: Ensure all `*/reduce' functions share the same API.
(defun set-reduce (acc f xs)
  "Return a new set by calling F on each element of XS and ACC."
  (->> xs
       set-to-list
       (-reduce-from (lambda (acc x) (funcall f x acc)) acc)))

(defun set-intersection (a b)
  "Return the set intersection between A and B."
  (set-reduce (set-new)
              (lambda (x acc)
                (if (set-contains? x b)
                    (set-add x acc)
                  acc))
              a))

(defun set-count (xs)
  "Return the number of elements in XS."
  (->> xs
       set-xs
       ht-size))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Predicates
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun set-empty? (xs)
  "Return t if XS has no elements in it."
  (= 0 (set-count xs)))

(defun set-contains? (x xs)
  "Return t if set XS has X."
  (ht-contains? (set-xs xs) x))

;; TODO: Prefer using `ht.el' functions for this.
(defun set-equal? (a b)
  "Return t if A and B share the name members."
  (ht-equal? (set-xs a)
             (set-xs b)))

(defun set-distinct? (a b)
  "Return t if A and B have no shared members."
  (set-empty? (set-intersection a b)))

(defun set-superset? (a b)
  "Return t if A has all of the members of B."
  (->> b
       set-to-list
       (-all? (lambda (x) (set-contains? x a)))))

(defun set-subset? (a b)
  "Return t if each member of set A is present in set B."
  (set-superset? b a))

(provide 'set)
;;; set.el ends here
