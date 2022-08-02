;;; struct.el --- Helpers for working with structs -*- lexical-binding: t -*-

;; Author: William Carroll <wpcarro@gmail.com>
;; Version: 0.0.1
;; Package-Requires: ((emacs "24.3"))

;;; Commentary:
;; Provides new macros for working with structs.  Also provides adapter
;; interfaces to existing struct macros, that should have more intuitive
;; interfaces.
;;
;; Sometimes `setf' just isn't enough.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dependencies
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 's)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Library
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro struct-update (type field f xs)
  "Apply F to FIELD in XS, which is a struct of TYPE.
This is immutable."
  (let ((copier (struct--copier-for type))
        (accessor (struct--accessor-for type field)))
    `(let ((copy (,copier ,xs)))
       (setf (,accessor copy) (funcall ,f (,accessor copy)))
       copy)))

(defmacro struct-update! (type field f xs)
  "Mutably apply F to FIELD in XS."
  (let ((accessor (struct--accessor-for type field)))
    `(progn
       (setf (,accessor ,xs) (funcall ,f (,accessor ,xs)))
       ,xs)))

(defmacro struct-set (type field x xs)
  "Immutably set FIELD in XS (struct TYPE) to X."
  (let ((copier (struct--copier-for type))
        (accessor (struct--accessor-for type field)))
    `(let ((copy (,copier ,xs)))
       (setf (,accessor copy) ,x)
       copy)))

(defmacro struct-set! (type field x xs)
  "Set FIELD in XS (struct TYPE) to X mutably.
This is an adapter interface to `setf'."
  (let ((accessor (struct--accessor-for type field)))
    `(progn
       (setf (,accessor ,xs) ,x)
       ,xs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helper Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun struct--copier-for (type)
  (intern (s-prepend "copy-" (symbol-name type))))

(defun struct--accessor-for (type field)
  (intern (s-prepend (s-concat (symbol-name type) "-")
                     (symbol-name field))))

(provide 'struct)
;;; struct.el ends here
