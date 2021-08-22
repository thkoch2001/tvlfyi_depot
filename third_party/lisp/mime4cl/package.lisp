;;;  package.lisp --- package declaration

;;;  Copyright (C) 2005-2007, 2010 by Walter C. Pelissero

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

(in-package :cl-user)

(defpackage :mime4cl
  (:nicknames :mime)
  (:use :common-lisp :npg :sclf
	;; for Gray streams
	#+cmu :extensions #+sbcl :sb-gray)
  ;; this is stuff that comes from SCLF and clashes with CMUCL's EXT
  ;; package
  (:shadowing-import-from :sclf
			  #:process-wait
			  #:process-alive-p
			  #:run-program)
  (:export #:*lazy-mime-decode*
	   #:print-mime-part
	   #:read-mime-message
	   #:mime-part
	   #:mime-text
	   #:mime-binary
	   #:mime-id
	   #:mime-image
	   #:mime-message
	   #:mime-multipart
	   #:mime-audio
	   #:mime-unknown-part
	   #:get-mime-disposition-parameter
	   #:get-mime-type-parameter
	   #:mime-disposition
	   #:mime-disposition-parameters
	   #:mime-encoding
	   #:mime-application
	   #:mime-video
	   #:mime-description
	   #:mime-part-size
	   #:mime-subtype
	   #:mime-body
	   #:mime-body-stream
	   #:mime-body-length
	   #:mime-parts
	   #:mime-part-p
	   #:mime-type
	   #:mime-type-string
	   #:mime-type-parameters
	   #:mime-message-headers
	   #:mime=
	   #:find-mime-part-by-path
	   #:find-mime-part-by-id
	   #:find-mime-text-part
	   #:encode-mime-part
	   #:encode-mime-body
	   #:decode-quoted-printable-stream
	   #:decode-quoted-printable-string
	   #:encode-quoted-printable-stream
	   #:encode-quoted-printable-sequence
	   #:decode-base64-stream
	   #:decode-base64-string
	   #:encode-base64-stream
	   #:encode-base64-sequence
	   #:parse-RFC2047-text
	   #:parse-RFC822-header
	   #:read-RFC822-headers
	   #:time-RFC822-string
	   #:parse-RFC822-date
	   #:map-parts
	   #:do-parts
	   #:apply-on-parts
	   #:mime-part-file-name
	   #:mime-text-charset
	   #:with-input-from-mime-body-stream
	   ;; endec.lisp
	   #:base64-encoder
	   #:base64-decoder
	   #:null-encoder
	   #:null-decoder
	   #:byte-encoder
	   #:byte-decoder
	   #:quoted-printable-encoder
	   #:quoted-printable-decoder
	   #:encoder-write-byte
	   #:encoder-finish-output
	   #:decoder-read-byte
	   #:decoder-read-sequence
	   #:*base64-line-length*
	   #:*quoted-printable-line-length*
	   ;; address.lisp
	   #:parse-addresses #:mailboxes-only
	   #:mailbox #:mbx-description #:mbx-user #:mbx-host #:mbx-domain #:mbx-domain-name #:mbx-address
	   #:mailbox-group #:mbxg-name #:mbxg-mailboxes))
