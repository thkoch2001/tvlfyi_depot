;;; tuple.el --- Tuple API for Elisp -*- lexical-binding: t -*-
;; Author: William Carroll <wpcarro@gmail.com>

;;; Commentary:
;; Work with cons cells with two elements with a familiar API for those who have
;; worked with tuples before.

;;; Code:

(cl-defstruct tuple first second)

;; Create
(defun tuple/new ()
  "Return an empty tuple."
  (make-tuple :first nil
              :second nil))

(defun tuple/from (a b)
  "Return a new tuple from A and B."
  (make-tuple :first a
              :second b))

(defun tuple/from-dotted (dp)
  "Convert dotted pair, DP, into a tuple."
  (tuple/from (car dp) (cdr dp)))

;; Read
(defun tuple/first (pair)
  "Return the first element of PAIR."
  (tuple-first pair))

(defun tuple/second (pair)
  "Return the second element of PAIR."
  (tuple-second pair))

;; Update
(defun tuple/map-each (f g pair)
  "Apply F to first, G to second in PAIR."
  (->> pair
       (tuple/map-first f)
       (tuple/map-second g)))

(defun tuple/map (f pair)
  "Apply F to PAIR."
  (let ((pair-copy (copy-tuple pair)))
    (funcall f pair-copy)))

(defun tuple/map-first (f pair)
  "Apply function F to the first element of PAIR."
  (let ((pair-copy (copy-tuple pair)))
    (setf (tuple-first pair-copy) (funcall f (tuple/first pair-copy)))
    pair-copy))

(defun tuple/map-second (f pair)
  "Apply function F to the second element of PAIR."
  (let ((pair-copy (copy-tuple pair)))
    (setf (tuple-second pair-copy) (funcall f (tuple/second pair-copy)))
    pair-copy))

(defun tuple/set-first (a pair)
  "Return a new tuple with the first element set as A in PAIR."
  (tuple/map-first (lambda (_) a) pair))

(defun tuple/set-second (b pair)
  "Return a new tuple with the second element set as B in PAIR."
  (tuple/map-second (lambda (_) b) pair))

;; Delete
(defun tuple/delete-first (pair)
  "Return PAIR with the first element set to nil."
  (tuple/set-first nil pair))

(defun tuple/delete-second (pair)
  "Return PAIR with the second element set to nil."
  (tuple/set-second nil pair))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Predicates
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun tuple/instance? (x)
  "Return t if X is a tuple."
  (tuple-p x))

(provide 'tuple)
;;; tuple.el ends here
