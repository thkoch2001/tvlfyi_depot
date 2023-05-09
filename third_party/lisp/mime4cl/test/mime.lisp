;;; mime.lisp --- MIME regression tests

;;; Copyright (C) 2012 by Walter C. Pelissero
;;; Copyright (C) 2021-2023 by the TVL Authors

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

(in-package :mime4cl-tests)

(defvar *samples-directory*
  (merge-pathnames (make-pathname :directory '(:relative "samples"))
                   #.(or *compile-file-pathname*
                         *load-pathname*
                         #P"")))

(loop
 for f in (directory (make-pathname :defaults *samples-directory*
                                    :name :wild
                                    :type "msg"))
 for i from 1
 do
 (add-test (intern (format nil "MIME.~A" i))
           `(let* ((orig (mime-message ,f))
                   (dup (mime-message
                         (with-output-to-string (out) (encode-mime-part orig out)))))
              (mime= orig dup))
           t))
