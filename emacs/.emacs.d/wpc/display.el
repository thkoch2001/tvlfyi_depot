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
(require 'dash)
(require 's)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Library
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl-defmacro display-register (name &key
                                    output
                                    primary
                                    coords
                                    size
                                    rate
                                    dpi
                                    rotate)
  "Macro to define constants and two functions for {en,dis}abling a display.

NAME    - the human-readable identifier for the display
OUTPUT  - the xrandr identifier for the display
PRIMARY - if true, send --primary flag to xrandr
COORDS  - X and Y offsets
SIZE    - the pixel resolution of the display
RATE    - the refresh rate
DPI     - the pixel density in dots per square inch
rotate  - one of {normal,left,right,inverted}

See the man-page for xrandr for more details."
  `(progn
     (defconst ,(intern (format "display-%s" name)) ,output
       ,(format "The xrandr identifier for %s" name))
     (defconst ,(intern (format "display-%s-args" name))
       ,(replace-regexp-in-string
         "\s+" " "
         (s-format "--output ${output} ${primary-flag} --auto \
                    --size ${size-x}x${size-y} --rate ${rate} --dpi ${dpi} \
                    --rotate ${rotate} ${pos-flag}"
                   #'aget
                   `(("output" . ,output)
                     ("primary-flag" . ,(if primary "--primary" "--noprimary"))
                     ("pos-flag" . ,(if coords
                                        (format "--pos %dx%d"
                                                (car coords)
                                                (cadr coords))
                                      ""))
                     ("size-x" . ,(car size))
                     ("size-y" . ,(cadr size))
                     ("rate" . ,rate)
                     ("dpi" . ,dpi)
                     ("rotate" . ,rotate))))
       ,(format "The arguments we pass to xrandr for display-%s." name))
     (defconst ,(intern (format "display-%s-command" name))
       (format "xrandr %s" ,(intern (format "display-%s-args" name)))
       ,(format "The command we run to configure %s" name))
     (defun ,(intern (format "display-enable-%s" name)) ()
       ,(format "Attempt to enable my %s monitor" name)
       (interactive)
       (prelude-start-process
        :name ,(format "display-enable-%s" name)
        :command ,(intern (format "display-%s-command" name))))
     (defun ,(intern (format "display-disable-%s" name)) ()
       ,(format "Attempt to disable my %s monitor." name)
       (interactive)
       (prelude-start-process
        :name ,(format "display-disable-%s" name)
        :command ,(format
                   "xrandr --output %s --off"
                   output)))))

(defmacro display-arrangement (name &key displays)
  "Create a function, display-arrange-<NAME>, to enable all your DISPLAYS."
  `(defun ,(intern (format "display-arrange-%s" name)) ()
     (interactive)
     (prelude-start-process
      :name ,(format "display-configure-%s" name)
      :command ,(format "xrandr %s"
                        (->> displays
                             (-map (lambda (x)
                                     (eval (intern (format "display-%s-args" x)))))
                             (s-join " "))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
                  :coords (0 1062)
                  :size (3840 2160)
                  :rate 30.0
                  :dpi 144
                  :rotate normal)

(display-register 4k-vertical
                  :output "DP2"
                  :primary nil
                  :coords (3840 0)
                  :size (3840 2160)
                  :rate 30.0
                  :dpi 144
                  :rotate right)

(display-arrangement primary
                     :displays (4k-horizontal 4k-vertical))

(provide 'display)
;;; display.el ends here
