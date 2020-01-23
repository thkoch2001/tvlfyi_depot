(in-package #:cl-user)
(defpackage #:server
  (:documentation "Robot condemned to a life of admin work for my blog.")
  (:use #:cl)
  (:export :main))
(in-package #:server)

(defun main ()
  "This is the main entrypoint for our application."
  (hunchentoot))
