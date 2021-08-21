;;;  mime4cl.asd --- system definition

;;;  Copyright (C) 2005-2007, 2010 by Walter C. Pelissero

;;;  Author: Walter C. Pelissero <walter@pelissero.de>
;;;  Project: mime4cl

;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License as
;;; published by the Free Software Foundation; either version 2, or (at
;;; your option) any later version.
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;; You should have received a copy of the GNU General Public License
;;; along with this program; see the file COPYING.  If not, write to
;;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;;; Boston, MA 02111-1307, USA.

(in-package :cl-user)

#+(and cmu (not gray-streams))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (ext:without-package-locks
    (load "library:subsystems/gray-streams-library")))

(defpackage :mime4cl-system
  (:use :common-lisp :asdf))

(in-package :mime4cl-system)

(defsystem mime4cl
    :name "MIME4CL"
    :author "Walter C. Pelissero <walter@pelissero.de>"
    :maintainer "Walter C. Pelissero <walter@pelissero.de>"
    ;; :version "0.0"
    :description "MIME primitives for Common Lisp"
    :long-description
    "A collection of Common Lisp primitives to forge and handle
MIME mail contents."
    :licence "LGPL"
    :depends-on (:npg :sclf)
    :components
    ((:file "package")
     (:file "mime" :depends-on ("package" "endec" "streams"))
     (:file "endec" :depends-on ("package"))
     (:file "streams" :depends-on ("package" "endec"))
     (:file "address" :depends-on ("package"))))

(defmethod perform ((o test-op) (c (eql (find-system 'mime4cl))))
  (oos 'load-op 'mime4cl-tests)
  (oos 'test-op 'mime4cl-tests :force t))
