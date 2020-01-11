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

;; TODO: Consider if this logic should be conditioned by `device/work-laptop?'.
(defconst display/laptop-monitor "eDP1"
  "The xrandr identifier for my primary screen (on work laptop).")

;; TODO: Why is HDMI-1, eDP-1 sometimes and HDMI1, eDP1 other times.
(defconst display/4k-monitor "HDMI1"
  "The xrandr identifer for my 4K monitor.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Library
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(provide 'display)
;;; display.el ends here
