;;; display.el --- Working with single or multiple displays -*- lexical-binding: t -*-

;; Author: William Carroll <wpcarro@gmail.com>
;; Version: 0.0.1
;; URL: https://git.wpcarro.dev/wpcarro/briefcase
;; Package-Requires: ((emacs "24"))

;;; Commentary:
;; Mostly wrappers around xrandr.
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
;; Library
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl-defmacro display-register (name &key
                                    output
                                    primary
                                    position
                                    size
                                    rate
                                    dpi
                                    rotate)
  "Macro to define a constant and two functions for {en,dis}abling a display.

NAME - the human-readable identifier for the display
OUTPUT - the xrandr identifier for the display
PRIMARY - if true, send --primary flag to xrandr
POSITION - one of {left-of,right-of,above,below,same-as}
SIZE - the pixel resolution of the display
RATE - the refresh rate
DPI - the pixel density in dots per square inch
rotate - one of {normal,left,right,inverted}

See the man-page for xrandr for more details."
  `(progn
     (defconst ,(intern (format "display-%s" name)) ,output
       ,(format "The xrandr identifier for %s" name))
     (defun ,(intern (format "display-enable-%s" name)) ()
         ,(format "Attempt to enable my %s monitor" name)
         (interactive)
       (prelude-start-process
        :name ,(format "display-enable-%s" name)
        :command ,(format
                   "xrandr --output %s --%s %s --auto --size %dx%d --rate %0.2f --dpi %d --rotate %s"
                   output
                   (if primary "primary" "noprimary")
                   (if position
                       (format "--%s %s"
                               (car position)
                               (eval (cadr position)))
                     "")
                   (car size) (cadr size)
                   rate
                   dpi
                   rotate)))
     (defun ,(intern (format "display-disable-%s" name)) ()
         ,(format "Attempt to disable my %s monitor." name)
         (interactive)
         (prelude-start-process
          :name ,(format "display-disable-%s" name)
          :command ,(format
                     "xrandr --output %s --off"
                     output)))))

;; I'm omitting the position argument to avoid a circular dependency between
;; laptop and 4k-horizontal.
(display-register laptop
                  :output "eDP1"
                  :primary nil
                  :size (3840 2160)
                  :rate 30.0
                  :dpi 144
                  :rotate normal)

(display-register 4k-horizontal
                  :output "HDMI1"
                  :primary t
                  :position (above display-laptop)
                  :size (3840 2160)
                  :rate 30.0
                  :dpi 144
                  :rotate normal)

(display-register 4k-vertical
                  :output "DP2"
                  :primary nil
                  :position (right-of display-4k-horizontal)
                  :size (3840 2160)
                  :rate 30.0
                  :dpi 144
                  :rotate right)

(provide 'display)
;;; display.el ends here
