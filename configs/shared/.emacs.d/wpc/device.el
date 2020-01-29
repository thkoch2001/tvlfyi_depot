;;; device.el --- Physical device information -*- lexical-binding: t -*-
;; Author: William Carroll <wpcarro@gmail.com>

;;; Commentary:
;; Functions for querying device information.

;;; Code:

(require 'dash)
(require 'alist)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Library
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst device/hostname->device
  '(("zeno.lon.corp.google.com" . work-desktop)
    ("seneca" . work-laptop))
  "Mapping hostname to a device symbol.")

;; TODO: Should I generate these predicates?

(defun device/classify ()
  "Return the device symbol for the current host or nil if not supported."
  (alist/get system-name device/hostname->device))

(defun device/work-laptop? ()
  "Return t if current device is work laptop."
  (equal 'work-laptop
         (device/classify)))

(defun device/work-desktop? ()
  "Return t if current device is work desktop."
  (equal 'work-desktop
         (device/classify)))

(provide 'device)
;;; device.el ends here
