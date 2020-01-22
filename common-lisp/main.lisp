(in-package #:cl-user)
(defpackage #:utils
  (:documentation "Some utility functions and macros to wet my beak.")
  (:use #:cl)
  (:shadow #:type))
(in-package #:utils)

(defmacro type (name in out)
  `(declaim (ftype (function ,in ,out) ,name)))

(defmacro comment (&rest _forms) nil)

(type add (int int) int)
(defun add (a b)
  (+ a b))
