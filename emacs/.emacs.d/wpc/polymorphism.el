;;; polymorphism.el --- Sketching my ideas for polymorphism in Elisp -*- lexical-binding: t -*-
;; Author: William Carroll <wpcarro@gmail.com>

;;; Commentary:
;; Once again: modelled after Elixir.

;;; Code:

;; More sketches of Elisp polymorphism initiative.
;;
;; Two macros:
;; - `defprotocol'
;; - `definstance'
;;
;; Is it just a coincidence that these two macros have the same number of
;;characters or is that fate?  I say fate.
;;
;; (defprotocol monoid
;;   :functions (empty concat))
;;
;; (definstance monoid vector
;;   :empty
;;   (lambda () [])
;;   :concat
;;   #'vector/concat)
;;
;; More sketching...
;; (defun monoid/empty ()
;;   "Sketch."
;;   (funcall #'(,(monoid/classify)/empty)))
;; (defun monoid/concat (xs)
;;   "Sketch."
;;   (apply #'(,(monoid/classify)/concat) args))


(provide 'polymorphism)
;;; polymorphism.el ends here
