(defpackage :maildir
  (:use :common-lisp)
  (:shadow :list)
  (:export :list)
  (:documentation
   "Very incomplete package for dealing with maildir(5)."))

(defpackage :mblog
  (:use
   :common-lisp
   :mime4cl
   :closure-html
   :who
   :uiop)
  (:shadow :with-html-output) ; conflict between closure-html and who
  (:import-from
   :alexandria
   :when-let*
   :when-let
   :starts-with-subseq
   :ends-with-subseq)
  (:export :main))
