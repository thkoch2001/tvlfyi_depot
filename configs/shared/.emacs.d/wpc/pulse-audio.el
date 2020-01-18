;;; pulse-audio.el --- Control audio with Elisp -*- lexical-binding: t -*-
;; Author: William Carroll <wpcarro@gmail.com>

;;; Commentary:
;; Because everything in my configuration is turning into Elisp these days.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dependencies
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'prelude)
(require 'string)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst pulse-audio/step-size 5
  "The size by which to increase or decrease the volume.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Library
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun pulse-audio/message (x)
  "Output X to *Messages*."
  (message (string/format "[pulse-audio.el] %s" x)))

(defun pulse-audio/toggle-mute ()
  "Mute the default sink."
  (interactive)
  (prelude/start-process
   :name "pulse-audio/toggle-mute"
   :command "pactl set-sink-mute @DEFAULT_SINK@ toggle")
  (pulse-audio/message "Mute toggled."))

(defun pulse-audio/toggle-microphone ()
  "Mute the default sink."
  (interactive)
  (prelude/start-process
   :name "pulse-audio/toggle-microphone"
   :command "pactl set-source-mute @DEFAULT_SOURCE@ toggle")
  (pulse-audio/message "Microphone toggled."))

(defun pulse-audio/decrease-volume ()
  "Low the volume output of the default sink."
  (interactive)
  (prelude/start-process
   :name "pulse-audio/decrease-volume"
   :command (string/format "pactl set-sink-volume @DEFAULT_SINK@ -%s%%"
                           pulse-audio/step-size))
  (pulse-audio/message "Volume decreased."))

(defun pulse-audio/increase-volume ()
  "Raise the volume output of the default sink."
  (interactive)
  (prelude/start-process
   :name "pulse-audio/increase-volume"
   :command (string/format "pactl set-sink-volume @DEFAULT_SINK@ +%s%%"
                           pulse-audio/step-size))
  (pulse-audio/message "Volume increased."))

(provide 'pulse-audio)
;;; pulse-audio.el ends here
