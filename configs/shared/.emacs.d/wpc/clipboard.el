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

(require 'bytes)

;; autoinsert feature feels unappealing at first attempt.
(use-package clipmon
  :config
  ;; If this is too large, it could be set machine-dependently, so use
  ;; `clipboard/print-clipboard-size' to help troubleshoot this if it becomes
  ;; problematic.
  (setq kill-ring-max 500)
  (add-to-list 'after-init-hook #'clipmon-mode-start)
  (clipmon-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Library
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar clipboard/install-kbds? t
  "When t, install keybindings.")

(defun clipboard/copy (x)
  "Copy string, X, to X11's clipboard."
  (kill-new x)
  (message "Copied!"))

(defun clipboard/paste ()
  "Paste contents of X11 clipboard."
  (yank)
  (message "Pasted!"))

(defun clipboard/print-clipboard-size ()
  "Message the size (in Bytes) of `kill-ring'."
  (interactive)
  (->> (clipmon-kill-ring-total)
       bytes/to-string
       message))

(defun clipboard/ivy-select ()
  "Use counsel to copy the selected entry to the system clipboard.
NOTE: A function, `counsel-yank-pop', exists that does something similar.
  However instead of copying the entry to the system clipboard, it inserts it
  where the current point is."
  (interactive)
  (ivy-read "kill-ring: " (counsel--yank-pop-kills)
            :require-match t
            :action #'clipboard/copy))

;; TODO: Support ivy-actions to insert into an Emacs buffer when an Emacs buffer
;; was the last active buffer.  However, if an X window is the last buffer,
;; maybe use xdotool to insert the selected entry.  This would be a bit of a
;; DWIM command.
(when clipboard/install-kbds?
  (exwm-input-set-key
   (kbd "C-M-v") #'clipboard/ivy-select))

(provide 'clipboard)
;;; clipboard.el ends here
