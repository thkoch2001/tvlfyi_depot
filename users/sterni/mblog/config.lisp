;; SPDX-License-Identifier: GPL-3.0-only
;; SPDX-FileCopyrightText: Copyright (C) 2023 by sterni

(in-package :config)

(eval-when (:compile-toplevel :load-toplevel)
  (defun plist-to-alist (lst)
    (loop for (name . (default . (parser . nil))) on lst by #'cdddr
          collect (cons name (list default parser))))

  (defun symbol-to-env-var-name (symbol)
    (concatenate 'string
                 "MBLOG_"
                 (string-upcase
                  (remove #\* (substitute #\_ #\- (string symbol)))))))

(defmacro define-configuration-variables (&rest args)
  (let ((vars (plist-to-alist args))
        (val-var-sym (gensym)))
    `(progn
       ,@(loop for (name . (default nil)) in vars
              collect `(defvar ,name ,default))

       (defun init-from-env ()
         ,@(loop for (name . (nil parser)) in vars
                 collect
                 `(when-let ((,val-var-sym (getenv ,(symbol-to-env-var-name name))))
                    (setf ,name (funcall ,parser ,val-var-sym))))))))

(define-configuration-variables
  *general-buffer-size* 4096 #'parse-integer)
