;; symbol.el --- Library for working with symbols. -*- lexical-binding: t -*-
;; Author: William Carroll <wpcarro@gmail.com>

;;; Commentary:
;; Library for working with symbols.

;;; Code:

;; TODO: Why is ivy mode map everywhere?

(require 'string)

;; Symbols
(defun symbol/as-string (callback x)
  "Treat the symbol, X, as a string while applying CALLBACK to it.
Coerce back to a symbol on the way out."
  (->> x
       #'symbol-name
       callback
       #'intern))

(defun symbol/to-string (x)
  "Map `X' into a string."
  (string/<-symbol x))

(defun symbol/hookify (x)
  "Append \"-hook\" to X when X is a symbol."
  (symbol/as-string #'string/hookify x))

(defun symbol/ensure-hookified (x)
  "Ensure that X has \"-hook\" appended to it when X is a symbol."
  (symbol/as-string #'string/ensure-hookified x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Predicates
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun symbol/instance? (x)
  "Return t if X is a symbol."
  (symbolp x))

(provide 'symbol)
;;; symbol.el ends here
