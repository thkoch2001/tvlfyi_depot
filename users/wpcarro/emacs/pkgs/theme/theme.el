;;; theme.el --- Colors and stuff -*- lexical-binding: t -*-

;; Author: William Carroll <wpcarro@gmail.com>
;; Version: 0.0.1
;; Package-Requires: ((emacs "24.3"))

;;; Commentary:
;;
;; Cycle through a whitelist of themes.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dependencies
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'cycle)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Library
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgroup theme nil
  "Customization options for `theme'."
  :group 'theme)

(defcustom theme-whitelist
  (cycle-from-list (custom-available-themes))
  "The whitelist of themes through which to cycle."
  :type '(cycle symbol)
  :group 'theme)

(defcustom theme-after-change
  nil
  "Hook invoked after a new theme is loaded"
  :type 'hook
  :group 'theme)

(defun theme-whitelist-set (theme)
  "Focus the THEME in the `theme-whitelist' cycle."
  (cycle-focus! (lambda (x) (equal x theme)) theme-whitelist)
  (theme--set (cycle-current theme-whitelist)))

(defun theme-select ()
  "Load a theme using `completing-read'."
  (interactive)
  (let ((theme (completing-read "Theme: " (cycle-to-list theme-whitelist))))
    (theme--disable-all)
    (theme--set (intern theme))))

(defun theme-next ()
  "Disable the currently active theme and load the next theme."
  (interactive)
  (disable-theme (cycle-current theme-whitelist))
  (theme--set (cycle-next! theme-whitelist))
  (message (format "Active theme: %s" (cycle-current theme-whitelist))))

(defun theme-prev ()
  "Disable the currently active theme and load the previous theme."
  (interactive)
  (disable-theme (cycle-current theme-whitelist))
  (theme--set (cycle-prev! theme-whitelist))
  (message (format "Active theme: %s" (cycle-current theme-whitelist))))

(defun theme--disable-all ()
  "Disable all currently enabled themes."
  (interactive)
  (dolist (x custom-enabled-themes)
    (disable-theme x)))

(defun theme--set (theme)
    "Call `load-theme' with `THEME', ensuring that the line numbers are bright.
There is no hook that I'm aware of to handle this more elegantly."
    (load-theme theme t)
    (run-hooks theme-on-change))

(provide 'theme)
;;; theme.el ends here
