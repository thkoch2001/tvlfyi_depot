;;; terminal.el --- My cobbled together terminal -*- lexical-binding: t -*-
;; Author: William Carroll <wpcarro@gmail.com>

;;; Commentary:
;; My attempts at creating a sane Emacs terminal.  Most of this work was created
;; before I discovered and fully adopted EXWM.  Prior to this, the appeal of
;; having terminals inside of Emacs was appealing.  So appealing in fact that I
;; was willing to work with inferior alternatives to non-Emacs terminals
;; (e.g. `ansi-term') instead of GUI alternatives like `alacritty` because the
;; productivity gains of having a terminal inside of Emacs might outweigh the
;; shortcomings of that particular terminal.
;;
;; All of this changed, however, after discovering EXWM, since I can embed X11
;; GUI windows inside of Emacs.  Therefore, most of this module is maintained
;; for historical purposes.
;;
;; Benefits of `ansi-term':
;; - Color scheme remains consistent between Emacs and terminal.
;; - Same applies to my fonts.
;;
;; Downsides of `ansi-term':
;; - Paging feels sluggish with programs like `cat` and `less`.
;; - KBDs don't provide 100% coverage of what I expect from a terminal since
;;   they were created to cooperate with Emacs.

;;; Code:

(require 'window)
(require 'buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Library
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: Model all open terminals within a dictionary.

(defconst wpc-terminal/name
  "wpc/terminal"
  "The name of my terminal buffers.")

(defun wpc-terminal/find-window ()
  "Return a reference to an existing terminal window or nil."
  (->> wpc-terminal/name
       wpc/add-earmuffs
       window/find))

(defun wpc-terminal/find-buffer ()
  "Return a reference to an existing terminal buffer."
  (->> wpc-terminal/name
       wpc/add-earmuffs
       buffer/find))

(defun wpc-terminal/find-or-create ()
  "Find or create a terminal window."
  (let ((buffer (wpc-terminal/find-buffer)))
    (if buffer
        (buffer/show buffer)
      (ansi-term "/usr/bin/zsh" wpc-terminal/name))))

;; TODO: Focus terminal after toggling it.
(defun wpc-terminal/toggle ()
  "Toggle a custom terminal session in Emacs."
  (interactive)
  (let ((window (wpc-terminal/find-window)))
    (if window
        (window/delete window)
      (wpc-terminal/find-or-create))))

(provide 'wpc-terminal)
;;; wpc-terminal.el ends here
