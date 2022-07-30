;;; symbol.el --- Library for working with symbols -*- lexical-binding: t -*-

;; Author: William Carroll <wpcarro@gmail.com>
;; Version: 0.0.1
;; Package-Requires: ((emacs "24"))

;;; Commentary:
;; Library for working with symbols.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Library
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun symbol-to-string (symbol)
  "Map `SYMBOL' into a string."
  (symbol-name symbol))

(defun symbol-from-string (string)
  "Map `STRING' into a symbol."
  (intern string))

(defun symbol-as-string (f x)
  "Treat the symbol, X, as a string while applying F to it.
Coerce back to a symbol on the way out."
  (symbol-from-string (funcall f (symbol-to-string x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Predicates
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun symbol-instance? (x)
  "Return t if X is a symbol."
  (symbolp x))

(provide 'symbol)
;;; symbol.el ends here
