(defpackage #:klatre
  (:documentation "Grab-bag utility library for Common Lisp")
  (:use #:cl)
  (:export
   ;; Miscellanious utilities
   #:comment #:posp

   ;; Sequence functions
   #:chunk-list #:mapconcat

   ;; String handling
   #:+dottime-format+ #:format-dottime
   #:try-parse-integer #:format-dottime-offset

   ;; Function utilities
   #:partial))
