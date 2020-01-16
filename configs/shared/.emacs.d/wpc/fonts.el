;;; fonts.el --- Font preferences -*- lexical-binding: t -*-
;; Author: William Carroll <wpcarro@gmail.com>

;;; Commentary:
;; Control my font preferences with ELisp.

;;; Code:

;; TODO: `defcustom' font-size.
;; TODO: `defcustom' fonts.
;; TODO: Remove wpc/ namespace.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dependencies
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'prelude)
(require 'cycle)
(require 'device)
(require 'maybe)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: Troubleshoot why "8" appears so large on my desktop.

;; TODO: Consider having a different font size when I'm using my 4K monitor.

(defconst fonts/size
  (pcase (device/classify)
    ('work-laptop  "9")
    ('work-desktop "8"))
  "My preferred default font-size, which is device specific.")

(defconst fonts/keybindings? t
  "Install the keybindings when non-nil.")

(defconst fonts/size-step 10
  "The amount (%) by which to increase or decrease a font.")

(defconst fonts/hacker-news-recommendations
  '("APL385 Unicode"
    "Go Mono"
    "Sudo"
    "Monoid"
    "Input Mono Medium" ;; NOTE: Also "Input Mono Thin" is nice.
    )
  "List of fonts optimized for programming I found in a HN article.")

(defconst fonts/whitelist
  (cycle/from-list
   (list/concat
    fonts/hacker-news-recommendations
    '("JetBrainsMono"
      "Mononoki Medium"
      "Monospace"
      "Operator Mono Light"
      "Courier"
      "Andale Mono"
      "Source Code Pro"
      "Terminus")))
  "This is a list of my preferred fonts.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: fonts and fonts/whitelist make it difficult to name functions like
;; fonts/set as a generic Emacs function vs choosing a font from the whitelist.

(cl-defun fonts/cycle (&key forward?)
  "Cycle forwards when `FORWARD?' non-nil."
  (let ((font (if forward?
                  (cycle/next fonts/whitelist)
                (cycle/prev fonts/whitelist))))
    (message (s-concat "Active font: " font))
    (fonts/set (fonts/fontify font))))

(defun fonts/next ()
  "Quickly cycle through preferred fonts."
  (interactive)
  (fonts/cycle :forward? t))

(defun fonts/prev ()
  "Quickly cycle through preferred fonts."
  (interactive)
  (fonts/cycle :forward? nil))

(defun fonts/set (font &optional size)
  "Change the font to `FONT' with option integer, SIZE, in pixels."
  (if (maybe/some? size)
      (set-frame-font (string/format "%s %s" font size) nil t)
    (set-frame-font font nil t)))

(defun fonts/whitelist-set (font)
  "Focuses the FONT in the `fonts/whitelist' cycle.
The size of the font is determined by `fonts/size'."
  (prelude/assert (cycle/contains? font fonts/whitelist))
  (cycle/focus (lambda (x) (equal x font)) fonts/whitelist)
  (fonts/set (fonts/current) fonts/size))

(defun fonts/ivy-select ()
  "Select a font from an ivy prompt."
  (interactive)
  (fonts/whitelist-set
   (ivy-read "Font: " (cycle/to-list fonts/whitelist))))

(defun fonts/print-current ()
  "Message the currently enabled font."
  (interactive)
  (message
   (string/format "[fonts] Current font: \"%s\""
                  (fonts/current))))

(defun fonts/current ()
  "Return the currently enabled font."
  (cycle/current fonts/whitelist))

(defun fonts/increase-size ()
  "Increase font size."
  (interactive)
  (->> (face-attribute 'default :height)
       (+ fonts/size-step)
       (set-face-attribute 'default (selected-frame) :height)))

(defun fonts/decrease-size ()
  "Decrease font size."
  (interactive)
  (->> (face-attribute 'default :height)
       (+ (- fonts/size-step))
       (set-face-attribute 'default (selected-frame) :height)))

(defun fonts/reset-size ()
  "Restore font size to its default value."
  (interactive)
  (fonts/whitelist-set (fonts/current)))

(when fonts/keybindings?
  (progn
    (evil-leader/set-key
      "Ff" #'fonts/next
      "Pf" #'fonts/prev)
    (general-define-key "s-9" #'fonts/ivy-select)
    (general-define-key "s-0" #'fonts/reset-size)
    (general-define-key "s-j" #'fonts/decrease-size)
    (general-define-key "s-k" #'fonts/increase-size)))

(provide 'fonts)
;;; fonts.el ends here
