;;; monoid.el --- Working with Monoids in Elisp -*- lexical-binding: t -*-
;; Author: William Carroll <wpcarro@gmail.com>

;;; Commentary:
;; The day has finally arrived where I'm using Monoids in Elisp.
;;
;; The monoid typeclass is as follows:
;; - empty :: a
;; - concat :: (list a) -> a

;;; Code:

;; TODO: Consider a prelude version that works for all Elisp types.
(defun monoid/classify (xs)
  "Return the type of `XS'."
  (cond
   ((listp xs) 'list)
   ((vectorp xs) 'vector)
   ((stringp xs) 'string)))


(defun monoid/empty (xs)
  "Return the empty monoid for the type `XS'."
  (pcase (monoid/classify xs)
    ('list '())
    ('vector [])
    ('string "")))

(provide 'monoid)
;;; monoid.el ends here
