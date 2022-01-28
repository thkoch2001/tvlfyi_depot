(defpackage :maildir
  (:use :common-lisp)
  (:shadow :list)
  (:export :list)
  (:documentation
   "Very incomplete package for dealing with maildir(5)."))

(defpackage :note
  (:use
   :common-lisp
   :babel
   :babel-encodings
   :closure-html
   :cl-date-time-parser
   :mime4cl)
  (:import-from
   :alexandria
   :when-let*
   :when-let
   :starts-with-subseq
   :ends-with-subseq)
  (:import-from :who :escape-string)
  (:export
   :apple-note
   :apple-note-uuid
   :apple-note-subject
   :apple-note-time
   :apple-note-text-part
   :make-apple-note
   :apple-note-html-fragment))

(defpackage :mblog
  (:use
   :common-lisp
   :uiop
   :note)
  (:export :main))
