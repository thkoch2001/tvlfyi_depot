;;; clipboard.el --- Working with X11's pasteboard -*- lexical-binding: t -*-
;; Author: William Carroll <wpcarro@gmail.com>

;;; Commentary:
;; Simple functions for copying and pasting.
;;
;; Integrate with bburns/clipmon so that System Clipboard can integrate with
;; Emacs's kill-ring.
;;
;; Wish list:
;; - Create an Emacs integration with github.com/cdown/clipmenud.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Library
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun clipboard/copy (x)
  "Copy string, X, to X11's clipboard."
  (kill-new x)
  (message "Copied!"))

(defun clipboard/paste ()
  "Paste contents of X11 clipboard."
  (yank)
  (message "Pasted!"))

(provide 'clipboard)
;;; clipboard.el ends here
