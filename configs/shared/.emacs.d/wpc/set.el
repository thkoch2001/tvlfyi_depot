;;; set.el --- Working with mathematical sets -*- lexical-binding: t -*-
;; Author: William Carroll <wpcarro@gmail.com>

;;; Commentary:
;; The set data structure is a collection that deduplicates its elements.

;;; Code:

(require 'ht) ;; friendlier API for hash-tables
(require 'dotted)
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

(defconst set/enable-testing? t
  "Run tests when t.")

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
                     (ht-set table-copy x nil)
                     table-copy))
                 xs))

;; TODO: Ensure all `*/reduce' functions share the same API.
(defun set/reduce (acc f xs)
  "Return a new set by calling F on each element of XS and ACC."
  (->> xs
       set/to-list
       (list/reduce acc f)))

(defun set/intersection (a b)
  "Return the set intersection between sets A and B."
  (set/reduce (set/new)
              (lambda (x acc)
                (if (set/contains? x b)
                    (set/add x acc)
                  acc))
              a))

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

;; TODO: Prefer using `ht.el' functions for this.
(defun set/equal? (a b)
  "Return t if A and B share the name members."
  (ht-equal? (set-xs a)
             (set-xs b)))

(defun set/distinct? (a b)
  "Return t if sets A and B have no shared members."
  (set/empty? (set/intersection a b)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when set/enable-testing?
  ;; set/distinct?
  (prelude/assert
   (set/distinct? (set/new 'one 'two 'three)
                  (set/new 'a 'b 'c)))
  (prelude/refute
   (set/distinct? (set/new 1 2 3)
                  (set/new 3 4 5)))
  (prelude/refute
   (set/distinct? (set/new 1 2 3)
                  (set/new 1 2 3)))
  ;; set/equal?
  (prelude/refute
   (set/equal? (set/new 'a 'b 'c)
               (set/new 'x 'y 'z)))
  (prelude/refute
   (set/equal? (set/new 'a 'b 'c)
               (set/new 'a 'b)))
  (prelude/assert
   (set/equal? (set/new 'a 'b 'c)
               (set/new 'a 'b 'c)))
  ;; set/intersection
  (prelude/assert
   (set/equal? (set/new 2 3)
               (set/intersection (set/new 1 2 3)
                                 (set/new 2 3 4))))
  ;; set/{from,to}-list
  (prelude/assert (equal '(1 2 3)
                         (->> '(1 1 2 2 3 3)
                              set/from-list
                              set/to-list)))
  ;; set/empty?
  (prelude/assert (set/empty? (set/new)))
  (prelude/refute (set/empty? (set/new 1 2 3)))
  ;; set/count
  (prelude/assert (= 0 (set/count (set/new))))
  (prelude/assert (= 2 (set/count (set/new 1 1 2 2)))))

(provide 'set)
;;; set.el ends here
