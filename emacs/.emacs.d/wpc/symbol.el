;;; symbol.el --- Library for working with symbols -*- lexical-binding: t -*-

;; Author: William Carroll <wpcarro@gmail.com>
;; Version: 0.0.1
;; URL: https://git.wpcarro.dev/wpcarro/briefcase
;; Package-Requires: ((emacs "24"))

;;; Commentary:
;; Library for working with symbols.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dependencies
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'string)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Library
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Symbols
(defun symbol-as-string (callback x)
  "Treat the symbol, X, as a string while applying CALLBACK to it.
Coerce back to a symbol on the way out."
  (->> x
       #'symbol-name
       callback
       #'intern))

(defun symbol-to-string (x)
  "Map `X' into a string."
  (string-<-symbol x))

(defun symbol-hookify (x)
  "Append \"-hook\" to X when X is a symbol."
  (symbol-as-string #'string-hookify x))

(defun symbol-ensure-hookified (x)
  "Ensure that X has \"-hook\" appended to it when X is a symbol."
  (symbol-as-string #'string-ensure-hookified x))

(defun symbol-instance? (x)
  "Return t if X is a symbol."
  (symbolp x))

(provide 'symbol)
;;; symbol.el ends here
