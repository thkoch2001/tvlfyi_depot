;;; window.el --- Working with Emacs windows -*- lexical-binding: t -*-
;; Author: William Carroll <wpcarro@gmail.com>

;;; Commentary:
;; Utilities to make CRUDing windows in Emacs easier.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dependencies
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'prelude)
(require 'macros)
(require 'maybe)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Library
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun window/find (name)
  "Find a window by the NAME of the buffer it's hosting."
  (let ((buffer (get-buffer name)))
    (if (maybe/some? buffer)
        (get-buffer-window buffer)
      nil)))

;; TODO: Find a way to incorporate these into function documentation.
(macros/comment
 (window/find "*scratch*"))

(defun window/delete (window)
  "Delete the WINDOW reference."
  (delete-window window))

(provide 'window)
;;; window.el ends here
