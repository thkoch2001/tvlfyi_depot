;;; colorscheme.el --- Syntax highlight and friends -*- lexical-binding: t -*-

;; Author: William Carroll <wpcarro@gmail.com>
;; Version: 0.0.1
;; URL: https://git.wpcarro.dev/wpcarro/briefcase
;; Package-Requires: ((emacs "24.3"))

;;; Commentary:
;;
;; TODO: Clarify this.
;; Since I have my own definition of "theme", which couples wallpaper, font,
;; with Emacs's traditional notion of the word "theme", I'm choosing to use
;; "colorscheme" to refer to *just* the notion of syntax highlight etc.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dependencies
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'cycle)
(require 'general)
(require '>)
(require 'cl-macs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcustom colorscheme-install-kbds? t
  "If non-nil, enable the keybindings.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Library
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcustom colorscheme-whitelist
  (cycle-from-list
   (->> (custom-available-themes)
        (list-map #'symbol-name)
        (list-filter (>-> (s-starts-with? "doom-")))
        (list-map #'intern)))
  "The whitelist of colorschemes through which to cycle.")

(defun colorscheme-current ()
  "Return the currently enabled colorscheme."
  (cycle-current colorscheme-whitelist))

(defun colorscheme-disable-all ()
  "Disable all currently enabled colorschemes."
  (interactive)
  (->> custom-enabled-themes
       (list-map #'disable-theme)))

(defun colorscheme-set (theme)
    "Call `load-theme' with `THEME', ensuring that the line numbers are bright.
There is no hook that I'm aware of to handle this more elegantly."
    (load-theme theme t)
    (prelude-set-line-number-color "#da5468"))

(defun colorscheme-whitelist-set (colorscheme)
  "Focus the COLORSCHEME in the `colorscheme-whitelist' cycle."
  (cycle-focus (lambda (x) (equal x colorscheme)) colorscheme-whitelist)
  (colorscheme-set (colorscheme-current)))

(defun colorscheme-ivy-select ()
  "Load a colorscheme using ivy."
  (interactive)
  (let ((theme (ivy-read "Theme: " (cycle-to-list colorscheme-whitelist))))
    (colorscheme-disable-all)
    (colorscheme-set (intern theme))))

(cl-defun colorscheme-cycle (&key forward?)
  "Cycle next if `FORWARD?' is non-nil.
Cycle prev otherwise."
  (disable-theme (cycle-current colorscheme-whitelist))
  (let ((theme (if forward?
                   (cycle-next colorscheme-whitelist)
                 (cycle-prev colorscheme-whitelist))))
    (colorscheme-set theme)
    (message (s-concat "Active theme: " (symbol-to-string theme)))))

(defun colorscheme-next ()
  "Disable the currently active theme and load the next theme."
  (interactive)
  (colorscheme-cycle :forward? t))

(defun colorscheme-prev ()
  "Disable the currently active theme and load the previous theme."
  (interactive)
  (colorscheme-cycle :forward? nil))

;; Keybindings
(when colorscheme-install-kbds?
  (general-define-key
   :prefix "<SPC>"
   :states '(normal)
   "Ft" #'colorscheme-next
   "Pt" #'colorscheme-prev))

(provide 'colorscheme)
;;; colorscheme.el ends here
