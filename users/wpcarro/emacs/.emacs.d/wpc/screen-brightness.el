;;; screen-brightness.el --- Control laptop screen brightness -*- lexical-binding: t -*-

;; Author: William Carroll <wpcarro@gmail.com>
;; Version: 0.0.1
;; URL: https://git.wpcarro.dev/wpcarro/briefcase
;; Package-Requires: ((emacs "24"))

;;; Commentary:
;; Control your laptop's screen brightness.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dependencies
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'prelude)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgroup screen-brightness nil "Configuration for screen-brightness.")

(defcustom screen-brightness-increase-cmd
  "light -A 3"
  "The shell command to run to increase screen brightness."
  :group 'screen-brightness
  :type 'string)

(defcustom screen-brightness-decrease-cmd
  "light -U 3"
  "The shell command to run to decrease screen brightness."
  :group 'screen-brightness
  :type 'string)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Library
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun screen-brightness-increase ()
  "Increase the screen brightness."
  (interactive)
  (prelude-start-process
   :name "screen-brightness-increase"
   :command screen-brightness-increase-cmd)
  (message "[screen-brightness.el] Increased screen brightness."))

(defun screen-brightness-decrease ()
  "Decrease the screen brightness."
  (interactive)
  (prelude-start-process
   :name "screen-brightness-decrease"
   :command screen-brightness-decrease-cmd)
  (message "[screen-brightness.el] Decreased screen brightness."))

(provide 'screen-brightness)
;;; screen-brightness.el ends here
