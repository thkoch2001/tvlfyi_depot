;;; buffer.el --- Working with Emacs buffers -*- lexical-binding: t -*-
;; Author: William Carroll <wpcarro@gmail.com>

;;; Commentary:
;; Utilities for CRUDing buffers in Emacs.
;;
;; Many of these functions may seem unnecessary especially when you consider
;; there implementations.  In general I believe that Elisp suffers from a
;; library disorganization problem.  Providing simple wrapper functions that
;; rename functions or reorder parameters is worth the effort in my opinion if
;; it improves discoverability (via intuition) and improve composability.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dependencies
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'maybe)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Library
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun buffer/exists? (name)
  "Return t if buffer, NAME, exists."
  (maybe/some? (buffer/find name)))

(defun buffer/find (buffer-or-name)
  "Find a buffer by its BUFFER-OR-NAME."
  (get-buffer buffer-or-name))

(defun buffer/new (name)
  "Return a newly created buffer NAME."
  (generate-new-buffer name))

(defun buffer/find-or-create (name)
  "Find or create buffer, NAME.
Return a reference to that buffer."
  (let ((x (buffer/find name)))
    (if (maybe/some? x)
        x
      (buffer/new name))))

;; TODO: Should this consume: `display-buffer' or `switch-to-buffer'?
(defun buffer/show (buffer-or-name)
  "Display the BUFFER-OR-NAME, which is either a buffer reference or its name."
  (display-buffer buffer-or-name))

(provide 'buffer)
;;; buffer.el ends here
