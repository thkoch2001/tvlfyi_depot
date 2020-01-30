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
;; Dependencies
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'prelude)
(require 'ivy-clipmenu)

(prelude/assert (prelude/executable-exists? "clipmenu"))
(prelude/assert (prelude/executable-exists? "clipmenud"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Library
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl-defun clipboard/copy (x &key (message "[clipboard.el] Copied!"))
  "Copy string, X, to X11's clipboard."
  (kill-new x)
  (message message))

(cl-defun clipboard/paste (&key (message "[clipboard.el] Pasted!"))
  "Paste contents of X11 clipboard."
  (yank)
  (message message))

(defun clipboard/contents ()
  "Return the contents of the clipboard as a string."
  (substring-no-properties (current-kill 0)))

(provide 'clipboard)
;;; clipboard.el ends here
