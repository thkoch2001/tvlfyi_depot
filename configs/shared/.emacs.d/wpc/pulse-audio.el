;;; pulse-audio.el --- Control audio with Elisp -*- lexical-binding: t -*-
;; Author: William Carroll <wpcarro@gmail.com>

;;; Commentary:
;; Because everything in my configuration is turning into Elisp these days.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst pulse-audio/install-kbds? t
  "When t, install keybindings defined herein.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Library
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun pulse-audio/toggle-mute ()
  "Mute the default sink."
  (interactive)
  (shell-command "pactl set-sink-mute @DEFAULT_SINK@ toggle")
  (message (string/format "[pulse-audio.el] Mute toggled.")))

(defun pulse-audio/lower-volume ()
  "Low the volume output of the default sink."
  (interactive)
  (shell-command "pactl set-sink-volume @DEFAULT_SINK@ -10%")
  (message (string/format "[pulse-audio.el] Volume lowered.")))

(defun pulse-audio/raise-volume ()
  "Raise the volume output of the default sink."
  (interactive)
  (shell-command "pactl set-sink-volume @DEFAULT_SINK@ +10%")
  (message (string/format "[pulse-audio.el] Volume raised.")))


(when pulse-audio/install-kbds?
  (exwm-input-set-key
   (kbd "<XF86AudioMute>") #'pulse-audio/toggle-mute)
  (exwm-input-set-key
   (kbd "<XF86AudioLowerVolume>") #'pulse-audio/lower-volume)
  (exwm-input-set-key
   (kbd "<XF86AudioRaiseVolume>") #'pulse-audio/raise-volume))

(provide 'pulse-audio)
;;; pulse-audio.el ends here
