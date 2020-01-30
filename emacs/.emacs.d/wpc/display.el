;;; display.el --- Working with single or multiple displays -*- lexical-binding: t -*-
;; Author: William Carroll <wpcarro@gmail.com>

;;; Commentary:
;; Mostly wrappers around xrandr.
;;
;; TODO: Look into autorandr to see if it could be useful.
;;
;; Troubleshooting:
;; The following commands help me when I (infrequently) interact with xrandr.
;; - xrandr --listmonitors
;; - xrandr --query

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dependencies
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'prelude)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst display/install-kbds? t
  "When t, install the keybindings defined in this module.")

;; TODO: Consider if this logic should be conditioned by `device/work-laptop?'.
(defconst display/laptop-monitor "eDP1"
  "The xrandr identifier for my primary screen (on work laptop).")

;; TODO: Why is HDMI-1, eDP-1 sometimes and HDMI1, eDP1 other times.
(defconst display/4k-monitor "HDMI1"
  "The xrandr identifer for my 4K monitor.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Library
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: Debug why something this scales to 4k appropriately and other times it
;; doesn't.
(defun display/enable-4k ()
  "Attempt to connect to my 4K monitor."
  (interactive)
  (prelude/start-process
   :name "display/enable-4k"
   :command (string/format
             "xrandr --output %s --above %s --primary --auto --dpi 144"
             display/4k-monitor
             display/laptop-monitor)))

(defun display/disable-4k ()
  "Disconnect from the 4K monitor."
  (interactive)
  (prelude/start-process
   :name "display/disable-4k"
   :command (string/format "xrandr --output %s --off"
                           display/4k-monitor)))

(defun display/enable-laptop ()
  "Turn the laptop monitor off.
Sometimes this is useful when I'm sharing my screen in a Google Hangout and I
  only want to present one of my monitors."
  (interactive)
  (prelude/start-process
   :name "display/disable-laptop"
   :command (string/format "xrandr --output %s --auto"
                           display/laptop-monitor)))

(defun display/disable-laptop ()
  "Turn the laptop monitor off.
Sometimes this is useful when I'm sharing my screen in a Google Hangout and I
  only want to present one of my monitors."
  (interactive)
  (prelude/start-process
   :name "display/disable-laptop"
   :command (string/format "xrandr --output %s --off"
                           display/laptop-monitor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keybindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when display/install-kbds?
  (general-define-key
   :prefix "<SPC>"
   :states '(normal)
   "d0" #'display/disable-laptop
   "d1" #'display/enable-laptop)
  (general-define-key
   :prefix "<SPC>"
   :states '(normal)
   "D0" #'display/disable-4k
   "D1" #'display/enable-4k))

(provide 'display)
;;; display.el ends here
