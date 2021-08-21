;;;  mime4cl-tests.asd --- system description for the regression tests

;;;  Copyright (C) 2006, 2007, 2010 by Walter C. Pelissero

;;;  Author: Walter C. Pelissero <walter@pelissero.de>
;;;  Project: mime4cl

;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Lesser General Public License
;;; as published by the Free Software Foundation; either version 2.1
;;; of the License, or (at your option) any later version.
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Lesser General Public License for more details.
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this library; if not, write to the Free
;;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
;;; 02111-1307 USA

#-(or sbcl cmu)
(warn "This code hasn't been tested on your Lisp system.")

(defpackage :mime4cl-tests-system
  (:use :common-lisp :asdf #+asdfa :asdfa)
  (:export #:*base-directory*
	   #:*compilation-epoch*))

(in-package :mime4cl-tests-system)

(defsystem mime4cl-tests
    :name "MIME4CL-tests"
    :author "Walter C. Pelissero <walter@pelissero.de>"
    :maintainer "Walter C. Pelissero <walter@pelissero.de>"
    :description "Test suite for the MIME4CL library"
    :long-description
    "These regression tests require rt.lisp from MIT.  It is included."
    :licence "LGPL"
    :depends-on (:mime4cl)
    :components
    ((:module test
	      :components
	      ((:file "rt")
	       (:file "package" :depends-on ("rt"))
	       (:file "endec" :depends-on ("rt" "package"))
	       (:file "address" :depends-on ("rt" "package"))
	       (:file "mime" :depends-on ("rt" "package"))))))

;; when loading this form the regression-test, the package is yet to
;; be loaded so we cannot use rt:do-tests directly or we would get a
;; reader error (unknown package)
(defmethod perform ((o test-op) (c (eql (find-system :mime4cl-tests))))
  (or (funcall (intern "DO-TESTS" "REGRESSION-TEST"))
      (error "test-op failed")))
