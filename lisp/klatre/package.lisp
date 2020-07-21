(defpackage #:klatre
  (:documentation "Grab-bag utility library for Common Lisp")
  (:use #:cl)
  (:export
   ;; Miscellanious utilities
   #:comment #:posp

   ;; Sequence functions
   #:chunk-list #:mapconcat

   ;; String handling
   #:try-parse-integer))
