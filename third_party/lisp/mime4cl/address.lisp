;;;  address.lisp --- e-mail address parser

;;;  Copyright (C) 2007, 2008, 2009 by Walter C. Pelissero

;;;  Author: Walter C. Pelissero <walter@pelissero.de>
;;;  Project: mime4cl

#+cmu (ext:file-comment "$Module: address.lisp $")

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

;;;  Although not MIME specific, this parser is often useful together
;;;  with the MIME primitives.  It should be able to parse the address
;;;  syntax described in RFC2822 excluding the obsolete syntax (see
;;;  RFC822).  Have a look at the test suite to get an idea of what
;;;  kind of addresses it can parse.

(in-package :mime4cl)

(defstruct (mailbox (:conc-name mbx-))
  description
  user
  host
  domain)

(defstruct (mailbox-group (:conc-name mbxg-))
  name
  mailboxes)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun write-mailbox-domain-name (addr &optional (stream *standard-output*))
  (when (eq :internet (mbx-domain addr))
    (write-char #\[ stream))
  (write-string (mbx-host addr) stream)
  (when (eq :internet (mbx-domain addr))
    (write-char #\] stream))
  (when (stringp (mbx-domain addr))
    (write-char #\. stream)
    (write-string (mbx-domain addr) stream)))

(defun write-mailbox-address (addr &optional (stream *standard-output*))
  (write-string (mbx-user addr) stream)
  (when (mbx-host addr)
    (write-char #\@ stream)
    (write-mailbox-domain-name addr stream)))

(defmethod mbx-domain-name ((MBX mailbox))
  "Return the complete domain name string of MBX, in the form
\"host.domain\"."
  (with-output-to-string (out)
    (write-mailbox-domain-name mbx out)))

(defmethod mbx-address ((mbx mailbox))
  "Return the e-mail address string of MBX, in the form
\"user@host.domain\"."
  (with-output-to-string (out)
    (write-mailbox-address mbx out)))

(defun write-mailbox (addr &optional (stream *standard-output*))
  (awhen (mbx-description addr)
    (write it :stream stream :readably t)
    (write-string " <" stream))
  (write-mailbox-address addr stream)
  (awhen (mbx-description addr)
    (write-char #\> stream)))

(defun write-mailbox-group (grp &optional (stream *standard-output*))
  (write-string (mbxg-name grp) stream)
  (write-string ": " stream)
  (loop
     for mailboxes on (mbxg-mailboxes grp)
     for mailbox = (car mailboxes)
     do (write-mailbox mailbox stream)
     unless (endp (cdr mailboxes))
     do (write-string ", " stream))
  (write-char #\; stream))

(defmethod print-object ((mbx mailbox) stream)
  (if (or *print-readably* *print-escape*)
      (call-next-method)
      (write-mailbox mbx stream)))

(defmethod print-object ((grp mailbox-group) stream)
  (if (or *print-readably* *print-escape*)
      (call-next-method)
      (write-mailbox-group grp stream)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun parser-make-mailbox (description address-list)
  (make-mailbox :description description
		:user (car address-list)
		:host (cadr address-list)
		:domain (when (cddr address-list)
			  (string-concat (cddr address-list) "."))))


(defun populate-grammar ()
  (defrule address-list
      := (+ address ","))

  (defrule address
      := mailbox
      := group)

  (defrule mailbox
      := display-name? angle-addr comment?
      :reduce (parser-make-mailbox (or display-name comment) angle-addr)
      := addr-spec comment?
      :reduce (parser-make-mailbox comment addr-spec))

  (defrule angle-addr
      := "<" addr-spec ">")

  (defrule group
      := display-name ":" mailbox-list ";"
      :reduce (make-mailbox-group :name display-name :mailboxes mailbox-list))

  (defrule display-name
      := phrase
      :reduce (string-concat phrase " "))

  (defrule phrase
      := word+)

  (defrule word
      := atext
      := string)

  (defrule mailbox-list
      := (+ mailbox ","))

  (defrule addr-spec
      := local-part "@" domain :reduce (cons local-part domain))

  (defrule local-part
      := dot-atom :reduce (string-concat dot-atom ".")
      := string)

  (defrule domain
      := dot-atom
      := domain-literal :reduce (list domain-literal :internet))

  ;; actually, according to the RFC, dot-atoms don't allow spaces in
  ;; between but these rules do
  (defrule dot-atom
      := (+ atom "."))

  (defrule atom
      := atext+
      :reduce (apply #'concatenate 'string atext)))

(deflazy define-grammar
  (let ((*package* #.*package*)
	(*compile-print* (when npg::*debug* t)))
    (reset-grammar)
    (format t "~&creating e-mail address grammar...~%")
    (populate-grammar)
    (let ((grammar (npg:generate-grammar #'string=)))
      (reset-grammar)
      (npg:print-grammar-figures grammar)
      grammar)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The lexical analyser

(defstruct cursor
  stream
  (position 0))

(defun read-delimited-string (stream end-char &key nesting-start-char (escape-char #\\))
  (labels ((collect ()
	     (with-output-to-string (out)
	       (loop
		  for c = (read-char stream nil)
		  while (and c (not (char= c end-char)))
		  do (cond ((char= c escape-char)
			    (awhen (read-char stream nil)
			      (write-char it out)))
			   ((and nesting-start-char
				 (char= c nesting-start-char))
			    (write-char nesting-start-char out)
			    (write-string (collect) out)
			    (write-char end-char out))
			   (t (write-char c out)))))))
    (collect)))


(defun read-string (cursor)
  (make-token :type 'string
	      :value (read-delimited-string (cursor-stream cursor) #\")
	      :position (incf (cursor-position cursor))))

(defun read-domain-literal (cursor)
  (make-token :type 'domain-literal
	      :value (read-delimited-string (cursor-stream cursor) #\])
	      :position (incf (cursor-position cursor))))

(defun read-comment (cursor)
  (make-token :type 'comment
	      :value (read-delimited-string (cursor-stream cursor) #\) :nesting-start-char #\()
	      :position (incf (cursor-position cursor))))

(declaim (inline atom-component-p))
(defun atom-component-p (c)
  (declare (type character c))
  (not (find c " ()\"[]@.<>:;,")))

(defun read-atext (first-character cursor)
  (be string (with-output-to-string (out)
	       (write-char first-character out)
	       (loop
		  for c = (read-char (cursor-stream cursor) nil)
		  while (and c (atom-component-p c))
		  do (write-char c out)
		  finally (when c
			    (unread-char c (cursor-stream cursor)))))
    (make-token :type 'atext
		:value string
		:position (incf (cursor-position cursor)))))

(defmethod read-next-tokens ((cursor cursor))
  (flet ((make-keyword (c)
	   (make-token :type 'keyword
		       :value (string c)
		       :position (incf (cursor-position cursor)))))
    (be in (cursor-stream cursor)
      (loop
	 for c = (read-char in nil)
	 while c
	 unless (whitespace-p c)
	 return (list
		 (cond ((char= #\( c)
			(read-comment cursor))
		       ((char= #\" c)
			(read-string cursor))
		       ((char= #\[ c)
			(read-domain-literal cursor))
		       ((find c "@.<>:;,")
			(make-keyword c))
		       (t
			;; anything else is considered a text atom even
			;; though it's just a single character
			(read-atext c cursor))))))))

(defun analyse-string (string)
  "Return the list of tokens produced by a lexical analysis of
STRING.  These are the tokens that would be seen by the parser."
  (with-input-from-string (stream string)
    (be cursor (make-cursor :stream stream)
      (loop
	 for tokens = (read-next-tokens cursor)
	 until (endp tokens)
	 append tokens))))

(defun mailboxes-only (list-of-mailboxes-and-groups)
  "Return a flat list of MAILBOX-ADDRESSes from
LIST-OF-MAILBOXES-AND-GROUPS, which is the kind of list returned
by PARSE-ADDRESSES.  This turns out to be useful when your
program is not interested in mailbox groups and expects the user
addresses only."
  (mapcan #'(lambda (mbx)
	      (if (typep mbx 'mailbox-group)
		  (mbxg-mailboxes mbx)
		  (list mbx)))
	  list-of-mailboxes-and-groups))

(defun parse-addresses (string &key no-groups)
  "Parse STRING and return a list of MAILBOX-ADDRESSes or
MAILBOX-GROUPs.  If STRING is unparsable return NIL.  If
NO-GROUPS is true, return a flat list of mailboxes throwing away
the group containers, if any."
  (be grammar (force define-grammar)
    (with-input-from-string (stream string)
      (be* cursor (make-cursor :stream stream)
	   mailboxes (ignore-errors	; ignore parsing errors
		       (parse grammar 'address-list cursor))
	(if no-groups
	    (mailboxes-only mailboxes)
	    mailboxes)))))

(defun debug-addresses (string)
  "More or less like PARSE-ADDRESSES, but don't ignore parsing errors."
  (be grammar (force define-grammar)
    (with-input-from-string (stream string)
      (be cursor (make-cursor :stream stream)
	(parse grammar 'address-list cursor)))))

