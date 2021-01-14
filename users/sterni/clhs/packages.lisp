(defpackage :clhs-lookup.clhs-path
  (:use :cl)
  (:export :*clhs-path*))

(defpackage clhs-lookup
  (:use :cl :uiop)
  (:import-from :clhs-lookup.clhs-path :*clhs-path*)
  (:export :main
           :find-symbols-paths))

