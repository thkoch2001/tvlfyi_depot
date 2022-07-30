;;; bag.el --- Working with bags (aka multi-sets) -*- lexical-binding: t -*-

;; Author: William Carroll <wpcarro@gmail.com>
;; Version: 0.0.1
;; Package-Requires: ((emacs "24.3"))

;;; Commentary:
;; What is a bag?  A bag should be thought of as a frequency table.  It's a way
;; to convert a list of something into a set that allows duplicates.  Isn't
;; allowing duplicates the whole thing with Sets?  Kind of.  But the interface
;; of Sets is something that bags resemble, so multi-set isn't as bad of a name
;; as it may first seem.
;;
;; If you've used Python's collections.Counter, the concept of a bag should be
;; familiar already.
;;
;; Interface:
;; - add        :: x -> Bag(x) -> Bag(x)
;; - remove     :: x -> Bag(x) -> Bag(x)
;; - union      :: Bag(x) -> Bag(x) -> Bag(x)
;; - difference :: Bag(x) -> Bag(x) -> Bag(x)

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dependencies
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'al)
(require 'list)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Library
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl-defstruct bag xs)

(defun bag-new ()
  "Create an empty bag."
  (make-bag :xs (al-new)))

(defun bag-from-list (xs)
  "Map a list of `XS' into a bag."
  (->> xs
       (list-reduce (bag-new) #'bag-add)))

(defun bag-add (x xs)
  "Add X to XS."
  (if (bag-contains? x xs)
      (struct-update
       bag xs (lambda (xs) (al-update x (lambda (x) (+ 1 x)) xs)) xs)
    (struct-update bag xs (lambda (xs) (al-set x 1 xs)) xs)))

(defun bag-remove (x xs)
  "Remove X from XS.
This is a no-op is X doesn't exist in XS."
  (when (bag-contains? x xs)
    (struct-update bag xs (lambda (xs) (al-delete x xs)) xs)))

(defun bag-count (x xs)
  "Return the number of occurrences of X in XS."
  (al-get x (bag-xs xs) 0))

(defun bag-total (xs)
  "Return the total number of elements in XS."
  (->> (bag-xs xs)
       (al-reduce 0 (lambda (_key v acc) (+ acc v)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Predicates
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun bag-contains? (x xs)
  "Return t if XS has X."
  (al-has-key? x (bag-xs xs)))

(provide 'bag)
;;; bag.el ends here
