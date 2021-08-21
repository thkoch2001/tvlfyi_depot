;;;  common.lisp --- common stuff

;;;  Copyright (C) 2003-2006, 2009 by Walter C. Pelissero

;;;  Author: Walter C. Pelissero <walter@pelissero.de>
;;;  Project: NPG a Naive Parser Generator

#+cmu (ext:file-comment "$Module: common.lisp $")

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

(in-package :naive-parser-generator)

(eval-when (:compile-toplevel :load-toplevel)
  (defstruct grammar
    rules
    keywords
    equal-p)

  (defstruct rule
    name
    productions)

  (defstruct (production (:conc-name prod-))
    tokens
    (tokens-length 0 :type fixnum)
    action)

  (defstruct token
    type		     ; type of token (identifier, number, ...)
    value				; its actual value
    position)			     ; line/column in the input stream
  ) ; eval-when

(defmethod print-object ((obj rule) stream)
  (format stream "#R(~A)" (rule-name obj)))

(defmethod print-object ((obj production) stream)
  (format stream "#P(action: ~S)" (prod-action obj)))

(defmethod print-object ((obj token) stream)
  (format stream "#T:~A=~S" (token-type obj) (token-value obj)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (inline make-rules-table find-rule add-rule))

(defun make-rules-table ()
  (make-hash-table))

(defun find-rule (rule-name rules)
  (gethash rule-name rules))

(defun add-rule (rule-name rule rules)
  (setf (gethash rule-name rules) rule))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (inline make-keywords-table find-keyword add-keyword))

(defun make-keywords-table ()
   (make-hash-table :test 'equal))

(defun find-keyword (keyword-name keywords)
  (gethash keyword-name keywords))

(defun add-keyword (keyword keywords)
  (setf (gethash keyword keywords) t))
