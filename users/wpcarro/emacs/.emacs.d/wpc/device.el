;;; device.el --- Physical device information -*- lexical-binding: t -*-

;; Author: William Carroll <wpcarro@gmail.com>
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1"))

;;; Commentary:
;; Functions for querying device information.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dependencies
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'dash)
(require 'al)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Library
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO(wpcarro): Consider integrating this with Nix and depot instead of
;; denormalizing the state.
(defconst device-hostname->device
  '(("zeno.lon.corp.google.com" . work-desktop-lon)
    ("wpcarro.svl.corp.google.com" . work-desktop-svl)
    ("seneca" . work-laptop)
    ("marcus" . personal-laptop)
    ("diogenes" . personal-vm))
  "Mapping hostname to a device symbol.")

;; TODO: Should I generate these predicates?

(defun device-classify ()
  "Return the device symbol for the current host or nil if not supported."
  (al-get system-name device-hostname->device))

(defun device-work-laptop? ()
  "Return t if current device is work laptop."
  (equal 'work-laptop
         (device-classify)))

(defun device-work-desktop? ()
  "Return t if current device is work desktop."
  (-contains? '(work-desktop-lon
                work-desktop-svl)
              (device-classify)))

(defun device-corporate? ()
  "Return t if the current device is owned by my company."
  (-contains? '(work-desktop-lon
                work-desktop-svl
                work-laptop)
              (device-classify)))

(defun device-laptop? ()
  "Return t if the current device is a laptop."
  (-contains? '(work-laptop personal-laptop) (device-classify)))

(provide 'device)
;;; device.el ends here
