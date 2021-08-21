;;;  npg.asd --- declaration of this system

;;;  Copyright (C) 2003, 2006 by Walter C. Pelissero

;;;  Author: Walter C. Pelissero <walter@pelissero.de>
;;;  Project: NPG a Naive Parser Generator

#+cmu (ext:file-comment "$Module: npg.asd, Time-stamp: <2006-01-03 17:20:21 wcp> $")

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

(defpackage :npg-system
  (:use :common-lisp :asdf))

(in-package :npg-system)

(defclass sample-file (doc-file) ())
(defmethod source-file-type ((c sample-file) (s module))
  "lisp")

(defsystem npg
  :name "NPG"
  :author "Walter C. Pelissero <walter@pelissero.de>"
  :maintainer "Walter C. Pelissero <walter@pelissero.de>"
  :licence "Lesser General Public License"
  :description "NPG a Naive Parser Generator"
  :long-description
  "NPG is a backtracking recursive descent parser generator for
Common Lisp. It accepts rules in a Lispy EBNF syntax without indirect
left recursive rules."
  :components
  ((:doc-file "README")
   (:doc-file "COPYING")
   (:doc-file ".project")
   (:module :examples
	    :components
	    ((:sample-file "python")
	     (:sample-file "vs-cobol-ii")))
   (:module :src
	    :components
	    ((:file "package")
	     (:file "common" :depends-on ("package"))
	     (:file "define" :depends-on ("package" "common"))
	     (:file "parser" :depends-on ("package" "common"))))))
