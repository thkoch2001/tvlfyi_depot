;;; playback.el --- Control playback with Elisp -*- lexical-binding: t -*-
;; Author: William Carroll <wpcarro@gmail.com>

;;; Commentary:
;; As you know, my whole universe is turning Elisp, so this should too!

;;; Code:

(defun playback/prev ()
  "Move to the previous song."
  (interactive)
  (shell-command "playerctl previous"))

(defun playback/next ()
  "Move to the next song."
  (interactive)
  (shell-command "playerctl next"))

(defun playback/play-pause ()
  "Play or pause the current song."
  (interactive)
  (shell-command "playerctl play-pause"))

(provide 'playback)
;;; playback.el ends here
