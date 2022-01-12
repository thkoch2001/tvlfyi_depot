(defpackage :maildir
  (:use :common-lisp)
  (:shadow :list)
  (:export :list)
  (:documentation
   "Very incomplete package for dealing with maildir(5)."))

(defpackage :note
  (:use
   :common-lisp
   :closure-html
   :who
   :cl-date-time-parser
   :mime4cl
   :who)
  (:import-from
   :alexandria
   :when-let*
   :when-let
   :starts-with-subseq
   :ends-with-subseq)
  (:shadow :with-html-output) ; conflict between closure-html and who
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
