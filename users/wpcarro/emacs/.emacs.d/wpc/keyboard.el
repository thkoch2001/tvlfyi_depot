;;; keyboard.el --- Managing keyboard preferences with Elisp -*- lexical-binding: t -*-

;; Author: William Carroll <wpcarro@gmail.com>
;; Version: 0.0.1
;; Package-Requires: ((emacs "24.3"))

;;; Commentary:
;; Setting key repeat and other values.
;;
;; Be wary of suspiciously round numbers.  Especially those divisible by ten!

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dependencies
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'cl-lib)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: Support clamping functions for repeat-{rate,delay} to ensure only valid
;; values are sent to xset.
(defcustom keyboard-repeat-rate 80
  "The number of key repeat signals sent per second.")

(defcustom keyboard-repeat-delay 170
  "The number of milliseconds before autorepeat starts.")

(defconst keyboard-repeat-rate-copy keyboard-repeat-rate
  "Copy of `keyboard-repeat-rate' to support `keyboard-reset-key-repeat'.")

(defconst keyboard-repeat-delay-copy keyboard-repeat-delay
  "Copy of `keyboard-repeat-delay' to support `keyboard-reset-key-repeat'.")

(defcustom keyboard-install-preferences? t
  "When t, install keyboard preferences.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun keyboard-message (x)
  "Message X in a structured way."
  (message (format "[keyboard.el] %s" x)))

(cl-defun keyboard-set-key-repeat (&key
                                   (rate keyboard-repeat-rate)
                                   (delay keyboard-repeat-delay))
  "Use xset to set the key-repeat RATE and DELAY."
  (prelude-start-process
   :name "keyboard-set-key-repeat"
   :command (format "xset r rate %s %s" delay rate)))

;; NOTE: Settings like this are machine-dependent. For instance I only need to
;; do this on my laptop and other devices where I don't have access to my split
;; keyboard.
;; NOTE: Running keysym Caps_Lock is not idempotent.  If this is called more
;; than once, xmodmap will start to error about non-existent Caps_Lock symbol.
;; For more information see here:
;; https://unix.stackexchange.com/questions/108207/how-to-map-caps-lock-as-the-compose-key-using-xmodmap-portably-and-idempotently
(defun keyboard-swap-caps-lock-and-escape ()
  "Swaps the caps lock and escape keys using xmodmap."
  (interactive)
  ;; TODO: Ensure these work once the tokenizing in prelude-start-process works
  ;; as expected.
  (start-process "keyboard-swap-caps-lock-and-escape"
                 nil "/usr/bin/xmodmap" "-e" "remove Lock = Caps_Lock")
  (start-process "keyboard-swap-caps-lock-and-escape"
                 nil "/usr/bin/xmodmap" "-e" "keysym Caps_Lock = Escape"))

(defun keyboard-inc-repeat-rate ()
  "Increment `keyboard-repeat-rate'."
  (interactive)
  (setq keyboard-repeat-rate (1+ keyboard-repeat-rate))
  (keyboard-set-key-repeat :rate keyboard-repeat-rate)
  (keyboard-message
   (format "Rate: %s" keyboard-repeat-rate)))

(defun keyboard-dec-repeat-rate ()
  "Decrement `keyboard-repeat-rate'."
  (interactive)
  (setq keyboard-repeat-rate (1- keyboard-repeat-rate))
  (keyboard-set-key-repeat :rate keyboard-repeat-rate)
  (keyboard-message
   (format "Rate: %s" keyboard-repeat-rate)))

(defun keyboard-inc-repeat-delay ()
  "Increment `keyboard-repeat-delay'."
  (interactive)
  (setq keyboard-repeat-delay (1+ keyboard-repeat-delay))
  (keyboard-set-key-repeat :delay keyboard-repeat-delay)
  (keyboard-message
   (format "Delay: %s" keyboard-repeat-delay)))

(defun keyboard-dec-repeat-delay ()
  "Decrement `keyboard-repeat-delay'."
  (interactive)
  (setq keyboard-repeat-delay (1- keyboard-repeat-delay))
  (keyboard-set-key-repeat :delay keyboard-repeat-delay)
  (keyboard-message
   (format "Delay: %s" keyboard-repeat-delay)))

(defun keyboard-print-key-repeat ()
  "Print the currently set values for key repeat."
  (interactive)
  (keyboard-message
   (format "Rate: %s. Delay: %s"
           keyboard-repeat-rate
           keyboard-repeat-delay)))

(defun keyboard-set-preferences ()
  "Reset the keyboard preferences to their default values.
NOTE: This function exists because occasionally I unplug and re-plug in a
  keyboard and all of the preferences that I set using xset disappear."
  (interactive)
  (keyboard-swap-caps-lock-and-escape)
  (keyboard-set-key-repeat :rate keyboard-repeat-rate
                           :delay keyboard-repeat-delay)
  ;; TODO: Implement this message function as a macro that pulls the current
  ;; file name.
  (keyboard-message "Keyboard preferences set!"))

(defun keyboard-reset-key-repeat ()
  "Set key repeat rate and delay to original values."
  (interactive)
  (keyboard-set-key-repeat :rate keyboard-repeat-rate-copy
                           :delay keyboard-repeat-delay-copy)
  (keyboard-message "Key repeat preferences reset."))

(when keyboard-install-preferences?
  (keyboard-set-preferences))

(provide 'keyboard)
;;; keyboard.el ends here
