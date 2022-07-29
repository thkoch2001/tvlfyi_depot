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
(require 'dash)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Library
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro struct-update (type field f xs)
  "Apply F to FIELD in XS, which is a struct of TYPE.
This is immutable."
  (let ((copier (->> type
                     symbol-name
                     (s-prepend "copy-")
                     intern))
        (accessor (->> field
                       symbol-name
                       (s-prepend (s-concat (symbol-name type) "-"))
                       intern)))
    `(let ((copy (,copier ,xs)))
       (setf (,accessor copy) (funcall ,f (,accessor copy)))
       copy)))

(defmacro struct-set (type field x xs)
  "Immutably set FIELD in XS (struct TYPE) to X."
  (let ((copier (->> type
                     symbol-name
                     (s-prepend "copy-")
                     intern))
        (accessor (->> field
                       symbol-name
                       (s-prepend (s-concat (symbol-name type) "-"))
                       intern)))
    `(let ((copy (,copier ,xs)))
       (setf (,accessor copy) ,x)
       copy)))

(defmacro struct-set! (type field x xs)
  "Set FIELD in XS (struct TYPE) to X mutably.
This is an adapter interface to `setf'."
  (let ((accessor (->> field
                       symbol-name
                       (s-prepend (s-concat (symbol-name type) "-"))
                       intern)))
    `(progn
       (setf (,accessor ,xs) ,x)
       ,xs)))

(provide 'struct)
;;; struct.el ends here
