;;; vterm-mgt.el --- Help me manage my vterm instances -*- lexical-binding: t -*-
;; Author: William Carroll <wpcarro@gmail.com>

;;; Commentary:
;; Supporting functions to instantiate vterm buffers, kill existing vterm
;; buffers, rename vterm buffers, cycle forwards and backwards through vterm
;; buffers.
;;
;; Many of the functions defined herein are intended to be bound to
;; `vterm-mode-map'.  Some assertions are made to guard against calling
;; functions that are intended to be called from outside of a vterm buffer.
;; These assertions shouldn't error when the functions are bound to
;; `vterm-mode-map'.  If for some reason, you'd like to bind these functions to
;; a separate keymap, caveat emptor.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dependencies
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'dash)
(require 'cycle)
(require 'vterm)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst vterm-mgt--instances (cycle/new)
  "A cycle tracking all of my vterm instances.")

(defcustom vterm-mgt-scroll-on-focus nil
  "When t, call `end-of-buffer' after focusing a vterm instance.")

(defun vterm-mgt--instance? (b)
  "Return t if the buffer B is a vterm instance."
  (equal 'vterm-mode (buffer-local-value 'major-mode b)))

(defmacro vterm-mgt--assert-vterm-buffer ()
  "Error when the `current-buffer' is not a vterm buffer."
  '(prelude/assert (vterm-mgt--instance? (current-buffer))))

(defun vterm-mgt-next ()
  "Replace the current buffer with the next item in `vterm-mgt--instances'.
This function should be called from a buffer running vterm."
  (interactive)
  (vterm-mgt--assert-vterm-buffer)
  (cycle/focus-item (current-buffer) vterm-mgt--instances)
  (switch-to-buffer (cycle/next vterm-mgt--instances))
  (when vterm-mgt-scroll-on-focus (end-of-buffer)))

(defun vterm-mgt-prev ()
  "Replace the current buffer with the previous item in `vterm-mgt--instances'.
This function should be called from a buffer running vterm."
  (interactive)
  (vterm-mgt--assert-vterm-buffer)
  (cycle/focus-item (current-buffer) vterm-mgt--instances)
  (switch-to-buffer (cycle/prev vterm-mgt--instances))
  (when vterm-mgt-scroll-on-focus (end-of-buffer)))

(defun vterm-mgt-instantiate ()
  "Create a new vterm instance.

Prefer calling this function instead of `vterm'.  This function ensures that the
  newly created instance is added to `vterm-mgt--instances'.

If however you must call `vterm', if you'd like to cycle through vterm
  instances, make sure you call `vterm-mgt-populate-cycle' to allow vterm-mgt to
  collect any untracked vterm instances."
  (interactive)
  (let ((buffer (vterm)))
    (cycle/append buffer vterm-mgt--instances)
    (cycle/focus-item buffer vterm-mgt--instances)))

(defun vterm-mgt-kill ()
  "Kill the current buffer and remove it from `vterm-mgt--instances'.
This function should be called from a buffer running vterm."
  (interactive)
  (vterm-mgt--assert-vterm-buffer)
  (let ((buffer (current-buffer)))
    (cycle/remove buffer vterm-mgt--instances)
    (kill-buffer buffer)))

(defun vterm-mgt-find-or-create ()
  "Call `switch-to-buffer' on a focused vterm instance if there is one.

When `cycle/focused?' returns nil, focus the first item in the cycle.  When
there are no items in the cycle, call `vterm-mgt-instantiate' to create a vterm
instance."
  (interactive)
  (if (cycle/empty? vterm-mgt--instances)
      (vterm-mgt-instantiate)
    (if (cycle/focused? vterm-mgt--instances)
        (switch-to-buffer (cycle/current vterm-mgt--instances))
      (progn
        (cycle/jump 0 vterm-mgt--instances)
        (switch-to-buffer (cycle/current vterm-mgt--instances))))))

(defun vterm-mgt-rename-buffer (name)
  "Rename the current buffer ensuring that its NAME is wrapped in *vterm*<...>.
This function should be called from a buffer running vterm."
  (interactive "SRename vterm buffer: ")
  (vterm-mgt--assert-vterm-buffer)
  (rename-buffer (format "vterm<%s>" name)))

(defun vterm-mgt-repopulate-cycle ()
  "Fill `vterm-mgt--instances' with the existing vterm buffers.

If for whatever reason, the state of `vterm-mgt--instances' is corrupted and
  misaligns with the state of vterm buffers in Emacs, use this function to
  attempt to restore the state."
  (interactive)
  (setq vterm-mgt--instances
        (->> (buffer-list)
             (-filter #'vterm-mgt--instance?)
             cycle/from-list)))

(provide 'vterm-mgt)
;;; vterm-mgt.el ends here
