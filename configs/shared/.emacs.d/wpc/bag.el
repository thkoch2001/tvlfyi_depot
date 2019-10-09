;;; bag.el --- Working with bags (aka multi-sets) -*- lexical-binding: t -*-
;; Author: William Carroll <wpcarro@gmail.com>

;;; Commentary:
;; What is a bag?  A bag should be thought of as a frequency table.  It's a way
;; to convert a list of something into a set that allows duplicates.  Isn't
;; allowing duplicates the whole thing with Sets?  Kind of.  But the interface
;; of Sets is something that bags resemble, so multi-set isn't as bag of a name
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

(require 'number)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Library
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl-defstruct bag xs)

(defun bag/update (f xs)
  "Call F on alist in XS."
  (let ((ys (bag-xs xs)))
    (setf (bag-xs xs) (funcall f ys))))

(defun bag/new ()
  "Create an empty bag."
  (make-bag :xs (alist/new)))

(defun bag/contains? (x xs)
  "Return t if XS has X."
  (alist/has-key? x (bag-xs xs)))

;; TODO: Tabling this for now since working with structs seems to be
;; disappointingly difficult.  Where is `struct/update'?
;; (defun bag/add (x xs)
;;   "Add X to XS.")

;; TODO: What do we name delete vs. remove?
;; (defun bag/remove (x xs)
;;   "Remove X from XS.
;; This is a no-op is X doesn't exist in XS.")

(defun bag/from-list (xs)
  "Map a list of `XS' into a bag."
  (->> xs
       (list/reduce
        (bag/new)
        (lambda (x acc)
          (bag/add x 1 #'number/inc acc)))))

(provide 'bag)
;;; bag.el ends here
