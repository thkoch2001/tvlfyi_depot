;;; temp-file.lisp --- temporary file creation

;;;  Copyright (C) 2005, 2006, 2007, 2008, 2009, 2010 by Walter C. Pelissero
;;;  Copyright (C) 2022 The TVL Authors

;;;  Author: Walter C. Pelissero <walter@pelissero.de>
;;;  Project: mime4cl
;;;
;;;  Code taken from SCLF

#+cmu (ext:file-comment "$Module: temp-file.lisp $")

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

(defvar *tmp-file-defaults* #P"/tmp/")

(defun temp-file-name (&optional (default *tmp-file-defaults*))
  "Create a random pathname based on DEFAULT.  No effort is made
to make sure that the returned pathname doesn't identify an
already existing file.  If missing DEFAULT defaults to
*TMP-FILE-DEFAULTS*."
  (make-pathname :defaults default
                 :name (format nil "~36R" (random #.(expt 36 10)))))

(defun open-temp-file (&optional default-pathname &rest open-args)
  "Open a new temporary file and return a stream to it.  This function
makes sure the pathname of the temporary file is unique.  OPEN-ARGS
are arguments passed verbatim to OPEN.  If OPEN-ARGS specify
the :DIRECTION it should be either :OUTPUT (default) or :IO;
any other value causes an error.  If DEFAULT-PATHNAME is specified and
not NIL it's used as defaults to produce the pathname of the temporary
file, otherwise *TMP-FILE-DEFAULTS* is used."
  (unless default-pathname
    (setf default-pathname *tmp-file-defaults*))
  ;; if :DIRECTION is specified check that it's compatible with the
  ;; purpose of this function, otherwise make it default to :OUTPUT
  (aif (getf open-args :direction)
       (unless (member it '(:output :io))
         (error "Can't create temporary file with open direction ~A." it))
       (setf open-args (append '(:direction :output)
                               open-args)))
  (do* ((name #1=(temp-file-name default-pathname) #1#)
        (stream #2=(apply #'open  name
                          :if-exists nil
                          :if-does-not-exist :create
                          open-args) #2#))
       (stream stream)))

(defmacro with-temp-file ((stream &rest open-temp-args) &body body)
  "Execute BODY within a dynamic extent where STREAM is bound to
a STREAM open on a unique temporary file name.  OPEN-TEMP-ARGS are
passed verbatim to OPEN-TEMP-FILE."
  `(be ,stream (open-temp-file ,@open-temp-args)
     (unwind-protect
          (progn ,@body)
       (close ,stream)
       ;; body may decide to rename the file so we must ignore the errors
       (ignore-errors
         (delete-file (pathname ,stream))))))
