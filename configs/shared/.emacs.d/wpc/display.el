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
;; Constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: Consider if this logic should be conditioned by `device/work-laptop?'.
(defconst display/primary "eDP-1"
  "The xrandr identifier for my primary screen (on work laptop).")

(defconst display/4k "HDMI-1"
  "The xrandr identifer for my 4K monitor.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Library
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun display/enable-4k ()
  "Attempt to connect to my 4K monitor."
  (interactive)
  (shell-command
   (string/format "xrandr --output %s --dpi 144 --auto --right-of %s"
                  display/4k
                  display/primary)))

(defun display/disable-4k ()
  "Disconnect from the 4K monitor."
  (interactive)
  (shell-command
   (string/format "xrandr --output %s --off"
                  display/4k)))

(provide 'display)
;;; display.el ends here
