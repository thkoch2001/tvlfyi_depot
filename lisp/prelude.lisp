(in-package #:cl-user)
(defpackage #:prelude
  (:documentation "Supporting miscellaneous utility functions and macros.")
  (:use #:cl)
  (:shadow #:type)
  (:export #:type #:comment))
(in-package #:prelude)

;; TODO: Add documentation to these macros.

(defmacro type (name in out)
  `(declaim (ftype (function ,in ,out) ,name)))

(defmacro comment (&rest _forms) nil)
