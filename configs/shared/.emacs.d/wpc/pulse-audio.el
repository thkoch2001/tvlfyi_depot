;;; pulse-audio.el --- Control audio with Elisp -*- lexical-binding: t -*-
;; Author: William Carroll <wpcarro@gmail.com>

;;; Commentary:
;; Because everything in my configuration is turning into Elisp these days.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dependencies
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'string)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst pulse-audio/step-size 5
  "The size by which to increase or decrease the volume.")

(defconst pulse-audio/install-kbds? t
  "When t, install keybindings defined herein.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Library
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun pulse-audio/message (x)
  "Output X to *Messages*."
  (message (string/format "[pulse-audio.el] %s" x)))

(defun pulse-audio/toggle-mute ()
  "Mute the default sink."
  (interactive)
  (start-process
   "*pactl<pulse-audio/toggle-mute>*"
   nil
   "pactl"
   "set-sink-mute"
   "@DEFAULT_SINK@"
   "toggle")
  (pulse-audio/message "Mute toggled."))

(defun pulse-audio/toggle-microphone ()
  "Mute the default sink."
  (interactive)
  (start-process
   "*pactl<pulse-audio/toggle-mute>*"
   nil
   "pactl"
   "set-source-mute"
   "@DEFAULT_SOURCE@"
   "toggle")
  (pulse-audio/message "Microphone toggled."))

(defun pulse-audio/decrease-volume ()
  "Low the volume output of the default sink."
  (interactive)
  (start-process
   "*pactl<pulse-audio/toggle-mute>*"
   nil
   "pactl"
   "set-sink-volume"
   "@DEFAULT_SINK@"
   (string/format "-%s%%" pulse-audio/step-size))
  (pulse-audio/message "Volume decreased."))

(defun pulse-audio/increase-volume ()
  "Raise the volume output of the default sink."
  (interactive)
  (start-process
   "*pactl<pulse-audio/toggle-mute>*"
   nil
   "pactl"
   "set-sink-volume"
   "@DEFAULT_SINK@"
   (string/format "+%s%%" pulse-audio/step-size))
  (pulse-audio/message "Volume increased."))

(when pulse-audio/install-kbds?
  (exwm-input-set-key
   (kbd "<XF86AudioMute>") #'pulse-audio/toggle-mute)
  (exwm-input-set-key
   (kbd "<XF86AudioLowerVolume>") #'pulse-audio/decrease-volume)
  (exwm-input-set-key
   (kbd "<XF86AudioRaiseVolume>") #'pulse-audio/increase-volume)
  (exwm-input-set-key
   (kbd "<XF86AudioMicMute>") #'pulse-audio/toggle-microphone))

(provide 'pulse-audio)
;;; pulse-audio.el ends here
