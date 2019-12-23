;;; wallpaper.el --- Control Linux desktop wallpaper -*- lexical-binding: t -*-
;; Author: William Carroll <wpcarro@gmail.com>

;;; Commentary:
;; Functions for setting desktop wallpaper.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dependencies
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'fs)
(require 'cycle)
(require 'string)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Library
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcustom wallpaper/keybindings? t
  "If non-nil, install the keybindings.")

(defcustom wallpaper/path-to-dir
  (f-expand "~/.local/share/wallpaper")
  "Path to the images that will be used as the wallpaper.")

(defconst wallpaper/whitelist
  (cycle/from-list
   (fs/ls wallpaper/path-to-dir t))
  "My preferred computer wallpapers.")

(defun wallpaper/set (path)
  "Set computer wallpaper to image at `PATH' using `feh` under-the-hood.
`PATH' can be absolute or relative since `f-expand' is called in the function
  body to ensure feh can resolve the path."
  (start-process "*feh<wallpaper/set>*"
                 nil
                 "feh"
                 "--bg-scale"
                 "--no-feh-bg"
                 (f-expand path)))

(defun wallpaper/whitelist-set (wallpaper)
  "Focuses the WALLPAPER in the `wallpaper/whitelist' cycle."
  (cycle/focus (lambda (x) (equal x wallpaper)) wallpaper/whitelist)
  (wallpaper/set (wallpaper/current)))

(defun wallpaper/next ()
  "Cycles to the next wallpaper."
  (interactive)
  (let ((wallpaper (cycle/next wallpaper/whitelist)))
    (wallpaper/set wallpaper)
    (message (string/format "Active wallpaper: %s" (f-filename wallpaper)))))

(defun wallpaper/prev ()
  "Cycles to the previous wallpaper."
  (interactive)
  (let ((wallpaper (cycle/prev wallpaper/whitelist)))
    (wallpaper/set wallpaper)
    (message (string/format "Active wallpaper: %s" (f-filename wallpaper)))))

;; TODO: Define a macro that handles, next, prev, select, current for working
;; with cycles, since this is a common pattern.

(defun wallpaper/print-current ()
  "Message the currently enabled wallpaper."
  (interactive)
  (message
   (cycle/current wallpaper/whitelist)))

(defun wallpaper/current ()
  "Return the currently enabled wallpaper."
  (cycle/current wallpaper/whitelist))

(defun wallpaper/ivy-select ()
  "Use `counsel' to select and set a wallpaper from the `wallpaper/whitelist'."
  (interactive)
  (wallpaper/whitelist-set
   (ivy-read "Select wallpaper: " (cycle/to-list wallpaper/whitelist))))

;; TODO: Create macro-based module system that will auto-namespace functions,
;; constants, etc. with the filename like `wallpaper'.

(when wallpaper/keybindings?
  (evil-leader/set-key
    "Fw" #'wallpaper/next
    "Pw" #'wallpaper/prev))

(provide 'wallpaper)
;;; wallpaper.el ends here
