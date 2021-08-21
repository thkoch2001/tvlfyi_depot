 ;;; eds.lisp --- En/De-coding Streams

 ;;; Copyright (C) 2012 by Walter C. Pelissero

 ;;; Author: Walter C. Pelissero <walter@pelissero.de>
 ;;; Project: mime4cl

#+cmu (ext:file-comment "$Module: eds.lisp")

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

(in-package :mime4cl)

#+cmu
(eval-when (:load-toplevel :compile-toplevel :execute)
  ;; CMUCL doesn't provide the STREAM-FILE-POSITION method in its
  ;; implementation of Gray streams.  We patch it in ourselves.
  (defgeneric stream-file-position (stream &optional position))
  (defun my-file-position (stream &optional position)
    (stream-file-position stream position))
  (defvar *original-file-position-function*
    (prog1
	(symbol-function 'file-position)
      (setf (symbol-function 'file-position) (symbol-function 'my-file-position))))
  (defmethod stream-file-position (stream &optional position)
    (if position
	(funcall *original-file-position-function* stream position)
	(funcall *original-file-position-function* stream)))

  ;; oddly CMUCL doesn't seem to provide a default for STREAM-READ-SEQUENCE
  (defmacro make-read-sequence (stream-type element-reader)
    `(defmethod stream-read-sequence ((stream ,stream-type) seq &optional start end)
       (unless start
	 (setf start 0))
       (unless end
	 (setf end (length seq)))
       (loop
	  for i from start below end
	  for b = (,element-reader stream)
	  until (eq b :eof)
	  do (setf (elt seq i) b)
	  finally (return i))))

  (make-read-sequence fundamental-binary-input-stream stream-read-byte)
  (make-read-sequence fundamental-character-input-stream stream-read-char))

(defclass coder-stream-mixin ()
  ((real-stream :type stream
		:initarg :stream
		:reader real-stream)
   (dont-close :initform nil
	       :initarg :dont-close)))

(defmethod stream-file-position ((stream coder-stream-mixin) &optional position)
  (file-position (slot-value stream 'real-stream) position))

(defclass coder-input-stream-mixin (fundamental-binary-input-stream coder-stream-mixin)
  ())
(defclass coder-output-stream-mixin (fundamental-binary-output-stream coder-stream-mixin)
  ())


(defclass quoted-printable-decoder-stream (coder-input-stream-mixin quoted-printable-decoder) ())
(defclass base64-decoder-stream (coder-input-stream-mixin base64-decoder) ())
(defclass 8bit-decoder-stream (coder-input-stream-mixin 8bit-decoder) ())

(defclass quoted-printable-encoder-stream (coder-output-stream-mixin quoted-printable-encoder) ())
(defclass base64-encoder-stream (coder-output-stream-mixin base64-encoder) ())
(defclass 8bit-encoder-stream (coder-output-stream-mixin 8bit-encoder) ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod initialize-instance :after ((stream coder-stream-mixin) &key &allow-other-keys)
  (unless (slot-boundp stream 'real-stream)
    (error "REAL-STREAM is unbound.  Must provide a :STREAM argument.")))

(defmethod initialize-instance ((stream coder-output-stream-mixin) &key &allow-other-keys)
  (call-next-method)
  (unless (slot-boundp stream 'output-function)
    (setf (slot-value stream 'output-function)
	  #'(lambda (char)
	      (write-char char (slot-value stream 'real-stream))))))

(defmethod initialize-instance ((stream coder-input-stream-mixin) &key &allow-other-keys)
  (call-next-method)
  (unless (slot-boundp stream 'input-function)
    (setf (slot-value stream 'input-function)
	  #'(lambda ()
	      (read-char (slot-value stream 'real-stream) nil)))))

(defmethod stream-read-byte ((stream coder-input-stream-mixin))
  (or (decoder-read-byte stream)
      :eof))

(defmethod stream-write-byte ((stream coder-output-stream-mixin) byte)
  (encoder-write-byte stream byte))

(defmethod close ((stream coder-stream-mixin) &key abort)
  (with-slots (real-stream dont-close) stream
    (unless dont-close
      (close real-stream :abort abort))))

(defmethod close ((stream coder-output-stream-mixin) &key abort)
  (unless abort
    (encoder-finish-output stream))
  (call-next-method))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass encoder-input-stream (fundamental-character-input-stream coder-stream-mixin)
  ((encoder)
   (buffer-queue :initform (make-queue)))
  (:documentation
   "This is the base class for encoders with the direction swapped. It
reads from REAL-STREAM a stream of bytes, encodes it and returnes it
in a stream of character."))

(defclass quoted-printable-encoder-input-stream (encoder-input-stream) ())
(defclass base64-encoder-input-stream (encoder-input-stream) ())
(defclass 8bit-encoder-input-stream (fundamental-character-input-stream coder-stream-mixin) ())

(defmethod initialize-instance ((stream quoted-printable-encoder-input-stream) &key &allow-other-keys)
  (call-next-method)
  (with-slots (encoder buffer-queue) stream
    (setf encoder
	  (make-instance 'quoted-printable-encoder
			 :output-function #'(lambda (char)
					      (queue-append buffer-queue char))))))

(defmethod initialize-instance ((stream base64-encoder-input-stream) &key &allow-other-keys)
  (call-next-method)
  (with-slots (encoder buffer-queue) stream
    (setf encoder
	  (make-instance 'base64-encoder
			 :output-function #'(lambda (char)
					      (queue-append buffer-queue char))))))

(defmethod stream-read-char ((stream encoder-input-stream))
  (with-slots (encoder buffer-queue real-stream) stream
    (loop
       while (queue-empty-p buffer-queue)
       do (be byte (read-byte real-stream nil)
	    (if byte
		(encoder-write-byte encoder byte)
		(progn
		  (encoder-finish-output encoder)
		  (queue-append buffer-queue :eof)))))
    (queue-pop buffer-queue)))


(defmethod stream-read-char ((stream 8bit-encoder-input-stream))
  (with-slots (real-stream) stream
    (aif (read-byte real-stream nil)
	 (code-char it)
	 :eof)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass input-adapter-stream ()
  ((source :initarg :source)
   (real-stream)
   (input-function)))

(defclass binary-input-adapter-stream (fundamental-binary-input-stream input-adapter-stream) ())

(defclass character-input-adapter-stream (fundamental-character-input-stream input-adapter-stream) ())

(defmethod stream-element-type ((stream binary-input-adapter-stream))
  '(unsigned-byte 8))

(defmethod initialize-instance ((stream input-adapter-stream) &key &allow-other-keys)
  (call-next-method)
  (assert (slot-boundp stream 'source)))

(defmethod initialize-instance ((stream binary-input-adapter-stream) &key &allow-other-keys)
  (call-next-method)
  ;; REAL-STREAM slot is set only if we are going to close it later on
  (with-slots (source real-stream input-function) stream
    (etypecase source
      (string
       (setf real-stream (make-string-input-stream source)
	     input-function #'(lambda ()
				(awhen (read-char real-stream nil)
				  (char-code it)))))
      ((vector (unsigned-byte 8))
       (be i 0
	 (setf input-function #'(lambda ()
				  (when (< i (length source))
				    (prog1 (aref source i)
				      (incf i)))))))
      (stream
       (assert (input-stream-p source))
       (setf input-function (if (subtypep (stream-element-type source) 'character)
				#'(lambda ()
				    (awhen (read-char source nil)
				      (char-code it)))
				#'(lambda ()
				    (read-byte source nil)))))
      (pathname
       (setf real-stream (open source :element-type '(unsigned-byte 8))
	     input-function #'(lambda ()
				(read-byte real-stream nil))))
      (file-portion
       (setf real-stream (open-decoded-file-portion source)
	     input-function #'(lambda ()
				(read-byte real-stream nil)))))))

(defmethod initialize-instance ((stream character-input-adapter-stream) &key &allow-other-keys)
  (call-next-method)
  ;; REAL-STREAM slot is set only if we are going to close later on
  (with-slots (source real-stream input-function) stream
    (etypecase source
      (string
       (setf real-stream (make-string-input-stream source)
	     input-function #'(lambda ()
				(read-char real-stream nil))))
      ((vector (unsigned-byte 8))
       (be i 0
	 (setf input-function #'(lambda ()
				  (when (< i (length source))
				    (prog1 (code-char (aref source i))
				      (incf i)))))))
      (stream
       (assert (input-stream-p source))
       (setf input-function (if (subtypep (stream-element-type source) 'character)
				#'(lambda ()
				    (read-char source nil))
				#'(lambda ()
				    (awhen (read-byte source nil)
				      (code-char it))))))
      (pathname
       (setf real-stream (open source :element-type 'character)
	     input-function #'(lambda ()
				(read-char real-stream nil))))
      (file-portion
       (setf real-stream (open-decoded-file-portion source)
	     input-function #'(lambda ()
				(awhen (read-byte real-stream nil)
				  (code-char it))))))))

(defmethod close ((stream input-adapter-stream) &key abort)
  (when (slot-boundp stream 'real-stream)
    (with-slots (real-stream) stream
      (close real-stream :abort abort))))

(defmethod stream-read-byte ((stream binary-input-adapter-stream))
  (with-slots (input-function) stream
    (or (funcall input-function)
	:eof)))

(defmethod stream-read-char ((stream character-input-adapter-stream))
  (with-slots (input-function) stream
    (or (funcall input-function)
	:eof)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass delimited-input-stream (fundamental-character-input-stream coder-stream-mixin)
  ((start-offset :initarg :start
		 :initform 0
		 :reader stream-start
		 :type integer)
   (end-offset :initarg :end
	       :initform nil
	       :reader stream-end
	       :type (or null integer))))

(defmethod print-object ((object delimited-input-stream) stream)
  (if *print-readably*
      (call-next-method)
      (with-slots (start-offset end-offset) object
	(print-unreadable-object (object stream :type t :identity t)
	  (format stream "start=~A end=~A" start-offset end-offset)))))

(defun base-stream (stream)
  (if (typep stream 'delimited-input-stream)
      (base-stream (real-stream stream))
      stream))

(defmethod initialize-instance ((stream delimited-input-stream) &key &allow-other-keys)
  (call-next-method)
  (unless (slot-boundp stream 'real-stream)
    (error "REAL-STREAM is unbound.  Must provide a :STREAM argument."))
  (with-slots (start-offset) stream
    (when start-offset
      (file-position stream start-offset))))

(defmethod stream-read-char ((stream delimited-input-stream))
  (with-slots (real-stream end-offset) stream
    (if (or (not end-offset)
	    (< (file-position real-stream) end-offset))
	(or (read-char real-stream nil)
	    :eof)
	:eof)))

#+(OR)(defmethod stream-read-byte ((stream delimited-input-stream))
  (with-slots (real-stream end-offset) stream
    (if (or (not end-offset)
	    (< (file-position real-stream) end-offset))
	(or (read-byte real-stream nil)
	    :eof)
	:eof)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass my-string-input-stream (fundamental-character-input-stream coder-stream-mixin)
  ((string :initarg :string
	   :reader stream-string)))

(defmethod initialize-instance ((stream my-string-input-stream) &key &allow-other-keys)
  (call-next-method)
  (assert (slot-boundp stream 'string))
  (with-slots (string real-stream) stream
    (setf real-stream (make-string-input-stream string))))

(defmethod stream-read-char ((stream my-string-input-stream))
  (with-slots (real-stream) stream
    (or (read-char real-stream nil)
	:eof)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct file-portion
  data					;  string or a pathname
  encoding
  start
  end)

(defun open-file-portion (file-portion)
  (be data (file-portion-data file-portion)
    (etypecase data
      (pathname
       (be stream (open data)
	 (make-instance 'delimited-input-stream
			:stream stream
			:start (file-portion-start file-portion)
			:end (file-portion-end file-portion))))
      (string
       (make-instance 'delimited-input-stream
		      :stream (make-string-input-stream data)
		      :start (file-portion-start file-portion)
		      :end (file-portion-end file-portion)))
      (stream
       (make-instance 'delimited-input-stream
		      :stream data
		      :dont-close t
		      :start (file-portion-start file-portion)
		      :end (file-portion-end file-portion))))))

(defun open-decoded-file-portion (file-portion)
  (make-instance (case (file-portion-encoding file-portion)
		   (:quoted-printable 'quoted-printable-decoder-stream)
		   (:base64 'base64-decoder-stream)
		   (t '8bit-decoder-stream))
		 :stream (open-file-portion file-portion)))
