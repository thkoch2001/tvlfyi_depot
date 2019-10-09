;;; ivy-helpers.el --- More interfaces to ivy -*- lexical-binding: t -*-
;; Author: William Carroll <wpcarro@gmail.com>

;;; Commentary:
;; Hopefully to improve my workflows.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dependencies
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'alist)
(require 'tuple)
(require 'string)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Library
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl-defun ivy-helpers/kv (prompt kv f)
  "PROMPT users with the keys in KV and return its corresponding value.  Calls F
with the key and value from KV."
  (ivy-read
   prompt
   kv
   :require-match t
   :action (lambda (entry)
             (funcall f (car entry) (cdr entry)))))

;;; Code:
(provide 'ivy-helpers)
;;; ivy-helpers.el ends here
