;; functions.el --- String helper functions for my Emacs development -*- lexical-binding: t -*-
;; Author: William Carroll <wpcarro@gmail.com>

;;; Commentary:
;; String & Symbol helpers!

;;; Code:

(require 'dash)
(require 's)

;; Strings
(defun string/hookify (x)
  "Append \"-hook\" to X."
  (s-append "-hook" x))

(defun symbol->string (symbol)
  "Alias for `symbol-name' with SYMBOL, since I can never remember that
function's name."
  (symbol-name symbol))

(defun string->symbol (string)
  "Alias for `intern' for STRING since I can never remember that function's
  name."
  (intern string))

(defun string/ensure-hookified (x)
  "Ensure that X has \"-hook\" appended to it."
  (if (s-ends-with? "-hook" x)
      x
    (string/hookify x)))

;; Symbols
(defun symbol/as-string (callback x)
  "Treat the symbol, X, as a string while applying CALLBACK to it.
Coerce back to a symbol on the way out."
  (->> x
       symbol-name
       callback
       intern))

(defun symbol/hookify (x)
  "Append \"-hook\" to X when X is a symbol."
  (symbol/as-string #'string/hookify x))

(defun symbol/ensure-hookified (x)
  "Ensure that X has \"-hook\" appended to it when X is a symbol."
  (symbol/as-string #'string/ensure-hookified x))

(provide 'string-functions)
;;; string-functions.el ends here
