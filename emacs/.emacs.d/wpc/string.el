;; string.el --- Library for working with strings -*- lexical-binding: t -*-

;; Author: William Carroll <wpcarro@gmail.com>
;; Version: 0.0.1
;; URL: https://git.wpcarro.dev/wpcarro/briefcase
;; Package-Requires: ((emacs "24"))

;;; Commentary:
;; Library for working with string.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dependencies
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 's)
(require 'dash)
;; TODO: Resolve the circular dependency that this introduces.
;; (require 'prelude)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Library
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun string-contains? (c x)
  "Return t if X is in C."
  (s-contains? c x))

(defun string-hookify (x)
  "Append \"-hook\" to X."
  (s-append "-hook" x))

(defun string-split (y x)
  "Map string X into a list of strings that were separated by Y."
  (s-split y x))

(defun string-ensure-hookified (x)
  "Ensure that X has \"-hook\" appended to it."
  (if (s-ends-with? "-hook" x)
      x
    (string-hookify x)))

(defun string-format (x &rest args)
  "Format template string X with ARGS."
  (apply #'format (cons x args)))

(defun string-concat (&rest strings)
  "Joins `STRINGS' into onto string."
  (apply #'s-concat strings))

(defun string-->symbol (string)
  "Maps `STRING' to a symbol."
  (intern string))

(defun string-<-symbol (symbol)
  "Maps `SYMBOL' into a string."
  (symbol-name symbol))

(defun string-prepend (prefix x)
  "Prepend `PREFIX' onto `X'."
  (s-concat prefix x))

(defun string-append (postfix x)
  "Appen `POSTFIX' onto `X'."
  (s-concat x postfix))

(defun string-surround (s x)
  "Surrounds `X' one each side with `S'."
  (->> x
       (string-prepend s)
       (string-append s)))

;; TODO: Define a macro for defining a function and a test.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Casing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun string-caps->kebab (x)
  "Change the casing of `X' from CAP_CASE to kebab-case."
  (->> x
       s-downcase
       (s-replace "_" "-")))

(defun string-kebab->caps (x)
  "Change the casing of X from CAP_CASE to kebab-case."
  (->> x
       s-upcase
       (s-replace "-" "_")))

(defun string-lower->caps (x)
  "Change the casing of X from lowercase to CAPS_CASE."
  (->> x
       s-upcase
       (s-replace " " "_")))

(defun string-lower->kebab (x)
  "Change the casing of `X' from lowercase to kebab-case."
  (s-replace " " "-" x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Predicates
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun string-instance? (x)
  "Return t if X is a string."
  (stringp x))

(provide 'string)
;;; string.el ends here
