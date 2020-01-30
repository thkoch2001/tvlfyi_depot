;;; struct.el --- Helpers for working with structs -*- lexical-binding: t -*-
;; Author: William Carroll <wpcarro@gmail.com>

;;; Commentary:
;; Provides new macros for working with structs.  Also provides adapter
;; interfaces to existing struct macros, that should have more intuitive
;; interfaces.
;;
;; Sometimes `setf' just isn't enough.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Wish list
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; - TODO: Replace `symbol-name' and `intern' calls with isomorphism.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dependencies
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'string)
(require 'dash)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Library
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar struct/enable-tests? t
  "When t, run the test suite defined herein.")

(defmacro struct/update (type field f xs)
  "Apply F to FIELD in XS, which is a struct of TYPE.
This is immutable."
  (let ((copier (->> type
                     symbol-name
                     (string/prepend "copy-")
                     intern))
        (accessor (->> field
                       symbol-name
                       (string/prepend (string/concat (symbol-name type) "-"))
                       intern)))
    `(let ((copy (,copier ,xs)))
       (setf (,accessor copy) (funcall ,f (,accessor copy)))
       copy)))

(defmacro struct/set (type field x xs)
  "Immutably set FIELD in XS (struct TYPE) to X."
  (let ((copier (->> type
                     symbol-name
                     (string/prepend "copy-")
                     intern))
        (accessor (->> field
                       symbol-name
                       (string/prepend (string/concat (symbol-name type) "-"))
                       intern)))
    `(let ((copy (,copier ,xs)))
       (setf (,accessor copy) ,x)
       copy)))

(defmacro struct/set! (type field x xs)
  "Set FIELD in XS (struct TYPE) to X mutably.
This is an adapter interface to `setf'."
  (let ((accessor (->> field
                       symbol-name
                       (string/prepend (string/concat (symbol-name type) "-"))
                       intern)))
    `(progn
       (setf (,accessor ,xs) ,x)
       ,xs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when struct/enable-tests?
  (cl-defstruct dummy name age)
  (defvar test-dummy (make-dummy :name "Roofus" :age 19))
  (struct/set! dummy name "Doofus" test-dummy)
  (prelude/assert (string= "Doofus" (dummy-name test-dummy)))
  (let ((result (struct/set dummy name "Shoofus" test-dummy)))
    ;; Test the immutability of `struct/set'
    (prelude/assert (string= "Doofus" (dummy-name test-dummy)))
    (prelude/assert (string= "Shoofus" (dummy-name result)))))

(provide 'struct)
;;; struct.el ends here
