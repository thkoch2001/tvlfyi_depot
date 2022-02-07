;;; dotted.el --- Working with dotted pairs in Elisp -*- lexical-binding: t -*-

;; Author: William Carroll <wpcarro@gmail.com>
;; Version: 0.0.1
;; Package-Requires: ((emacs "24.3"))

;;; Commentary:
;; Part of my primitives library extensions in Elisp.  Contrast my primitives
;; with the wrapper extensions that I provide, which expose immutable variants
;; of data structures like an list, alist, tuple, as well as quasi-typeclasses
;; like sequence, etc.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dependencies
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'macros)
(require 'cl-lib)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Library
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl-defun dotted-new (&optional a b)
  "Create a new dotted pair of A and B."
  (cons a b))

(defun dotted-instance? (x)
  "Return t if X is a dotted pair."
  (let ((b (cdr x)))
    (and b (atom b))))

(defun dotted-first (x)
  "Return the first element of X."
  (car x))

(defun dotted-second (x)
  "Return the second element of X."
  (cdr x))

(provide 'dotted)
;;; dotted.el ends here
