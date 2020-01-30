;;; zle.el --- Functions to mimmick my ZLE KBDs -*- lexical-binding: t -*-
;; Author: William Carroll <wpcarro@gmail.com>

;;; Commentary:
;; This is primarily for personal use.  The keybindings that I choose are those
;; that feel slightly mnemonic while also not shadowing important bindings.
;; It's quite possible that our tastes will differ here.
;;
;; All of these keybindings are intended to shave off milliseconds off your
;; typing.  I don't expect these numbers to sum up to a meaningful amount.  The
;; primary reason that I wrote this, is that it introduces a small amount of
;; structural editing to my workflow.  I've been using these exact keybindings
;; on the command line, and I find them subtely delightful to use.  So much so
;; that I decided to bring them to my Emacs configuration.
;;
;; ZLE is the Z-shell line editor.  I have some KBDs and functions that I often
;; want in Emacs.
;;
;; Usage:
;; Consider running `(zle-minor-mode)' to run this globally.  Depending on your
;; configuration, it could be non-disruptive, disruptive, or extremely
;; disruptive.
;;
;; TODO: Consider adding (general-unbind 'insert "C-v") herein.

;;; Code:

;; subshell (C-j)
(defun zle/subshell ()
  "Insert the characters necessary to create a subshell."
  (interactive)
  (insert-char ?$)
  (insert-char ?\()
  (save-excursion
    (insert-char ?\))))

;; variable (C-v)
(defun zle/variable ()
  "Insert the characters to reference a variable."
  (interactive)
  (insert-char ?$)
  (insert-char ?{)
  (save-excursion
    (insert-char ?})))

;; 2x dash (C-M--)
(defun zle/dash-dash ()
  "Insert the characters for flags with 2x dashes."
  (interactive)
  (insert-char ? )
  (insert-char ?-)
  (insert-char ?-))

;; 1x quotes (M-')
(defun zle/single-quote ()
  "Insert the characters to quickly create single quotes."
  (interactive)
  (insert-char ? )
  (insert-char ?')
  (save-excursion
    (insert-char ?')))

;; 2x quotes (M-")
(defun zle/double-quote ()
  "Insert the characters to quickly create double quotes."
  (interactive)
  (insert-char ? )
  (insert-char ?\")
  (save-excursion
    (insert-char ?\")))

(defvar zle/kbds
  (let ((map (make-sparse-keymap)))
    (bind-keys :map map
               ("C-j"   . zle/subshell)
               ("C-v"   . zle/variable)
               ("C-M--" . zle/dash-dash)
               ("M-'"   . zle/single-quote)
               ("M-\""  . zle/double-quote))
    map)
  "Keybindings shaving milliseconds off of typing.")

(define-minor-mode zle-minor-mode
  "A minor mode mirroring my ZLE keybindings."
  :init-value nil
  :lighter " zle"
  :keymap zle/kbds)

(provide 'zle)
;;; zle.el ends here
