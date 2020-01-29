(in-package #:cl-user)
(defpackage #:main
  (:documentation "Modern API for working with files and directories.")
  (:use #:cl)
  (:shadow #:type))
(in-package #:main)

;; Common Lisp distinguishes between `namestrings` and `pathnames` as two types
;; of filename representations.
;;
;; A `pathname` is a structured representation of the name of a file, which
;; consists of six parts:
;; 1. host
;; 2. device
;; 3. directory
;; 4. name
;; 5. type
;; 6. version

;; TODO: Should I be using `string` as a type or `namestring`?

(defmacro type (name in out)
  `(declaim (ftype (function ,in ,out) ,name)))

(type join (&rest namestring) pathname)
(defun join (&rest args)
  "Join ARGS to a single path."
  (apply #'merge-pathnames args))

(type ext (pathname) string)
(defun ext (path)
  "Return the file extension of PATH."
  (pathname-type path))

;; TODO: Define these tests elsewhere.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; join
(string= (join "path") "path")
(string= (join "path" "to") "path/to")
(string= (join "/" "path" "to" "heaven") "/path/to/heaven")

;; ext
(string= (ext #p"path/to/file.ext") "ext")
(string= (ext #p"path/to/directory") nil)
