;;; streams.lisp --- En/De-coding Streams

;;; Copyright (C) 2012 by Walter C. Pelissero
;;; Copyright (C) 2021-2022 by the TVL Authors

;;; Author: Walter C. Pelissero <walter@pelissero.de>
;;; Project: mime4cl

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

(defclass coder-stream-mixin ()
  ((real-stream :type stream
                :initarg :underlying-stream
                :reader real-stream)
   (dont-close :initform nil
               :initarg :dont-close)))

(defmethod stream-file-position ((stream coder-stream-mixin))
  (file-position (slot-value stream 'real-stream)))

(defmethod (setf stream-file-position) (newval (stream coder-stream-mixin))
  (file-position (slot-value stream 'real-stream) newval))

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
    (error "REAL-STREAM is unbound.  Must provide a :UNDERLYING-STREAM argument.")))

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

(defclass positioned-flexi-input-stream (flexi-input-stream)
  ((ignore-close
    :initform nil
    :initarg :ignore-close
    :documentation
    "If T, calling CLOSE on the stream does nothing.
If NIL, the underlying stream is closed."))
  (:documentation
   "FLEXI-INPUT-STREAM that automatically advances the underlying :STREAM to
the location given by :POSITION. This uses FILE-POSITION internally, so it'll
only works if the underlying stream position is tracked in bytes. Note that
the underlying stream is still advanced, so having multiple instances of
POSITIONED-FLEXI-INPUT-STREAM based with the same underlying stream won't work
reliably.
If :IGNORE-CLOSE is set, the underlying stream won't be closed if CLOSE is
called on the POSITIONED-FLEXI-INPUT-STREAM."))

(defmethod initialize-instance ((stream positioned-flexi-input-stream)
                                &key &allow-other-keys)
  (call-next-method)
  ;; The :POSITION initarg is only informational for flexi-streams: It assumes
  ;; it is were the stream it got is already at and continuously updates it
  ;; for querying (via FLEXI-STREAM-POSITION) and bound checking.
  ;; Since we have streams that are not positioned correctly, we need to do this
  ;; here using FILE-POSITION. Note that assumes the underlying implementation
  ;; uses bytes for FILE-POSITION which is not guaranteed (probably some streams
  ;; even in SBCL don't).
  (file-position (flexi-stream-stream stream) (flexi-stream-position stream)))

(defmethod close ((stream positioned-flexi-input-stream) &key abort)
  (declare (ignore abort))
  (with-slots (ignore-close) stream
    (unless ignore-close
      (call-next-method))))

(defun make-positioned-flexi-input-stream (stream &rest args)
  "Create a POSITIONED-FLEXI-INPUT-STREAM. Accepts the same keyword arguments as
MAKE-FLEXI-STREAM as well as :IGNORE-CLOSE. Causes the FILE-POSITION of STREAM to
be modified to match the :POSITION argument."
  (apply #'make-instance
         'positioned-flexi-input-stream
         :stream stream
         (mapcar (lambda (x)
                   ;; make-flexi-stream has a discrepancy between :initarg of
                   ;; make-instance and its &key which we mirror here.
                   (if (eq x :external-format) :flexi-stream-external-format x))
                 args)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO(sterni): test correct behavior with END NIL
(defstruct file-portion
  data                                  ; string or a pathname
  encoding
  start
  end)

(defun open-decoded-file-portion (file-portion)
  (with-slots (data encoding start end)
      file-portion
    (let* ((binary-stream
             (etypecase data
               (pathname
                (open data :element-type '(unsigned-byte 8)))
               (vector
                (flexi-streams:make-in-memory-input-stream data))
               (stream
                ;; TODO(sterni): assert that bytes/flexi-stream
                data)))
           (params (ccase encoding
                     ((:quoted-printable :base64) '(:external-format :us-ascii))
                     (:8bit '(:element-type (unsigned-byte 8)))
                     (:7bit '(:external-format :us-ascii))))
           (portion-stream (apply #'make-positioned-flexi-input-stream
                                  binary-stream
                                  :position start
                                  :bound end
                                  ;; if data is a stream we can't have a
                                  ;; FILE-PORTION without modifying it when
                                  ;; reading etc. The least we can do, though,
                                  ;; is forgo destroying it.
                                  :ignore-close (typep data 'stream)
                                  params))
           (needs-decoder-stream (member encoding '(:quoted-printable
                                                    :base64))))

      (if needs-decoder-stream
          (make-instance
           (ccase encoding
             (:quoted-printable 'quoted-printable-decoder-stream)
             (:base64 'base64-decoder-stream))
           :underlying-stream portion-stream)
          portion-stream))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun flexi-stream-root-stream (stream)
  "Return the non FLEXI-STREAM stream a given chain of FLEXI-STREAMs is based on."
  (if (typep stream 'flexi-stream)
      (flexi-stream-root-stream (flexi-stream-stream stream))
      stream))
