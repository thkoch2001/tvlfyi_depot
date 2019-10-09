;;; laptop-battery.el --- Display laptop battery information -*- lexical-binding: t -*-
;; Author: William Carroll <wpcarro@gmail.com>

;;; Commentary:
;; Some wrappers to obtain battery information.
;;
;; To troubleshoot battery consumpton look into the CLI `powertop`.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Roadmap
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: Support functions that work with reporting battery stats.
;; TODO: low-battery-reporting-threshold
;; TODO: charged-battery-reporting-threshold
;; TODO: Format modeline battery information.
;; TODO: Provide better time information in the modeline.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dependencies
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'battery)
(require 'alist)
(require 'maybe)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Library
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun laptop-battery/available? ()
  "Return t if battery information is available."
  (maybe/some? battery-status-function))

(defun laptop-battery/percentage ()
  "Return the current percentage of the battery."
  (->> battery-status-function
       funcall
       (alist/get 112)))

(defun laptop-battery/print-percentage ()
  "Return the current percentage of the battery."
  (interactive)
  (->> (laptop-battery/percentage)
       message))

(defun laptop-battery/display ()
  "Display laptop battery percentage in the modeline."
  (interactive)
  (display-battery-mode 1))

(defun laptop-battery/hide ()
  "Hide laptop battery percentage in the modeline."
  (interactive)
  (display-battery-mode -1))

(provide 'laptop-battery)
;;; laptop-battery.el ends here
