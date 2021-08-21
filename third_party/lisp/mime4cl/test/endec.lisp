;;;  endec.lisp --- test suite for the MIME encoder/decoder functions

;;;  Copyright (C) 2006, 2007, 2009, 2010 by Walter C. Pelissero

;;;  Author: Walter C. Pelissero <walter@pelissero.de>
;;;  Project: mime4cl

#+cmu (ext:file-comment "$Module: endec.lisp $")

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

(in-package :mime4cl-tests)

(deftest quoted-printable.1
    (encode-quoted-printable-sequence (map '(vector (unsigned-byte 8)) #'char-code
					   "Français, Español, böse, skøl"))
  "Fran=E7ais, Espa=F1ol, b=F6se, sk=F8l")

(deftest quoted-printable.2
    (encode-quoted-printable-sequence (map '(vector (unsigned-byte 8)) #'char-code
					   "Français, Español, böse, skøl")
				      :start 10 :end 17)
  "Espa=F1ol")

(deftest quoted-printable.3
    (map 'string #'code-char
	 (decode-quoted-printable-string "Fran=E7ais, Espa=F1ol, b=F6se, sk=F8l"))
  "Français, Español, böse, skøl")

(deftest quoted-printable.4
    (map 'string #'code-char
	 (decode-quoted-printable-string "Fran=E7ais, Espa=F1ol, b=F6se, sk=F8l"
					 :start 12 :end 21))
  "Español")

(deftest quoted-printable.5
    (map 'string #'code-char
	 (decode-quoted-printable-string "this = wrong"))
  "this = wrong")

(deftest quoted-printable.6
    (map 'string #'code-char
	 (decode-quoted-printable-string "this is wrong="))
  "this is wrong=")

(deftest quoted-printable.7
    (map 'string #'code-char
	 (decode-quoted-printable-string "this is wrong=1"))
  "this is wrong=1")

(deftest quoted-printable.8
    (encode-quoted-printable-sequence (map '(vector (unsigned-byte 8)) #'char-code
					   "x = x + 1"))
  "x =3D x + 1")

(deftest quoted-printable.9
    (encode-quoted-printable-sequence (map '(vector (unsigned-byte 8)) #'char-code
					   "x = x + 1   "))
  "x =3D x + 1  =20")

(deftest quoted-printable.10
    (encode-quoted-printable-sequence (map '(vector (unsigned-byte 8)) #'char-code
					   "this string is very very very very very very very very very very very very very very very very very very very very long"))
  "this string is very very very very very very very very very very very ve=
ry very very very very very very very very long")

(deftest quoted-printable.11
    (encode-quoted-printable-sequence (map '(vector (unsigned-byte 8)) #'char-code
					   "this string is very very                                                                                  very very long"))
  "this string is very very                                                =
                                  very very long")

(deftest quoted-printable.12
    (encode-quoted-printable-sequence (map '(vector (unsigned-byte 8)) #'char-code
					   "please read the next   
line"))
  "please read the next  =20
line")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest base64.1
    (let ((*base64-line-length* nil))
      (encode-base64-sequence (map '(vector (unsigned-byte 8)) #'char-code
				   "Some random string.")))
  "U29tZSByYW5kb20gc3RyaW5nLg==")

(deftest base64.2
    (let ((*base64-line-length* nil))
      (encode-base64-sequence (map '(vector (unsigned-byte 8)) #'char-code
				   "Some random string.") :start 5 :end 11))
  "cmFuZG9t")

(deftest base64.3
    (map 'string #'code-char
	 (decode-base64-string "U29tZSByYW5kb20gc3RyaW5nLg=="))
  "Some random string.")

(deftest base64.4
    (map 'string #'code-char
	 (decode-base64-string "some rubbish U29tZSByYW5kb20gc3RyaW5nLg== more rubbish"
			       :start 13 :end 41))
  "Some random string.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest RFC2047.1
    (parse-RFC2047-text "foo bar")
  ("foo bar"))

(defun perftest-encoder (encoder-class &optional (megs 100))
  (declare (optimize (speed 3) (debug 0) (safety 0))
	   (type fixnum megs))
  (with-open-file (in #P"/dev/random" :element-type '(unsigned-byte 8))
    (let* ((meg (* 1024 1024))
	   (buffer (make-sequence '(vector (unsigned-byte 8)) meg))
	   (encoder (make-instance encoder-class
				   :output-function #'(lambda (c) (declare (ignore c))))))
      (declare (type fixnum meg))
      (time
       (progn
	 (dotimes (x megs)
	   (read-sequence buffer in)
	   (dotimes (i meg)
	     (mime4cl:encoder-write-byte encoder (aref buffer i))))
	 (mime4cl:encoder-finish-output encoder))))))

(defun perftest-decoder (decoder-class &optional (megs 100))
  (declare (optimize (speed 3) (debug 0) (safety 0))
	   (type fixnum megs))
  (with-open-file (in #P"/dev/random" :element-type '(unsigned-byte 8))
    (let ((sclf:*tmp-file-defaults* (make-pathname :defaults #.(or *load-pathname* *compile-file-pathname*)
						   :type "encoded-data")))
      (sclf:with-temp-file (tmp nil :direction :io)
	(let* ((meg (* 1024 1024))
	       (buffer (make-sequence '(vector (unsigned-byte 8)) meg))
	       (encoder-class (ecase decoder-class
				(mime4cl:base64-decoder 'mime4cl:base64-encoder)
				(mime4cl:quoted-printable-decoder 'mime4cl:quoted-printable-encoder)))
	       (encoder (make-instance encoder-class
				       :output-function #'(lambda (c)
							    (write-char c tmp))))
	       (decoder (make-instance decoder-class
				       :input-function #'(lambda ()
							   (read-char tmp nil)))))
	  (declare (type fixnum meg))
	  (dotimes (x megs)
	    (read-sequence buffer in)
	    (dotimes (i meg)
	      (mime4cl:encoder-write-byte encoder (aref buffer i))))
	  (mime4cl:encoder-finish-output encoder)
	  (file-position tmp 0)
	  (time
	   (loop
	      for b = (mime4cl:decoder-read-byte decoder)
	      while b)))))))
