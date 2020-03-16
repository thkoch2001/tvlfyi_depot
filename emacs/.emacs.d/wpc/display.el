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
(require 'cycle)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: Consider if this logic should be conditioned by `device/work-laptop?'.
(defconst display/laptop-monitor "eDP1"
  "The xrandr identifier for my primary screen (on work laptop).")

;; TODO: Why is HDMI-1, eDP-1 sometimes and HDMI1, eDP1 other times.
(defconst display/4k-monitor "HDMI1"
  "The xrandr identifer for my 4K monitor.")

(defconst display/display-states (cycle/from-list '((t . nil) (nil . t)))
  "A list of cons cells modelling enabled and disabled states for my displays.
The car models the enabled state of my laptop display; the cdr models the
  enabled state of my external monitor.")

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
             "xrandr --output %s --above %s --primary --auto --size 3840x2160 --rate 30.00 --dpi 144"
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

(defun display/cycle-display-states ()
  "Cycle through `display/display-states' enabling and disabling displays."
  (interactive)
  (let ((state (cycle/next display/display-states)))
    (if (car state) (display/enable-laptop) (display/disable-laptop))
    (if (cdr state) (display/enable-4k) (display/disable-4k))))

(provide 'display)
;;; display.el ends here
