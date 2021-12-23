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
  '(("zeno.lon.corp.google.com" . work-desktop)
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
  (equal 'work-desktop
         (device-classify)))

(defun device-corporate? ()
  "Return t if the current device is owned by my company."
  (or (device-work-laptop?) (device-work-desktop?)))

(defun device-laptop? ()
  "Return t if the current device is a laptop."
  (-contains? '(work-laptop personal-laptop) (device-classify)))

(provide 'device)
;;; device.el ends here
