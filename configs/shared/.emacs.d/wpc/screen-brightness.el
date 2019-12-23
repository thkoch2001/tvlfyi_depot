;;; screen-brightness.el --- Control laptop screen brightness -*- lexical-binding: t -*-
;; Author: William Carroll <wpcarro@gmail.com>

;;; Commentary:
;; Mainly just Elisp wrappers around `xbacklight`.

;;; Code:

;; TODO: Define some isomorphisms. E.g. int->string, string->int.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dependencies
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'prelude)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst screen-brightness/step-size 15
  "The size of the increment or decrement step for the screen's brightness.")

(defcustom screen-brightness/install-kbds? t
  "If t, install the keybindings to control screen brightness.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Library
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun screen-brightness/increase ()
  "Increase the screen brightness."
  (interactive)
  (prelude/start-process
   :name "screen-brightness/increase"
   :command (string/format "xbacklight -inc %s" screen-brightness/step-size))
  (message "[screen-brightness.el] Increased screen brightness."))

(defun screen-brightness/decrease ()
  "Decrease the screen brightness."
  (interactive)
  (prelude/start-process
   :name "screen-brightness/decrease"
   :command (string/format "xbacklight -dec %s" screen-brightness/step-size))
  (message "[screen-brightness.el] Decreased screen brightness."))

(when screen-brightness/install-kbds?
  (exwm-input-set-key
   (kbd "<XF86MonBrightnessUp>") #'screen-brightness/increase)
  (exwm-input-set-key
   (kbd "<XF86MonBrightnessDown>") #'screen-brightness/decrease))

(provide 'screen-brightness)
;;; screen-brightness.el ends here
