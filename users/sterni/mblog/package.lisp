(defpackage :mblog
  (:use :cl :mime4cl :closure-html)
  (:import-from :alexandria
   :when-let* :when-let :starts-with-subseq)
  (:export :mnote-html))
