(defpackage :mblog
  (:use :cl :mime)
  (:import-from :alexandria :when-let* :when-let :starts-with-subseq :curry)
  (:export :mnote-html))
