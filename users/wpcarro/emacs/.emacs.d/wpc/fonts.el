;;; fonts.el --- Font preferences -*- lexical-binding: t -*-

;; Author: William Carroll <wpcarro@gmail.com>
;; Version: 0.0.1
;; Package-Requires: ((emacs "24.3"))

;;; Commentary:
;; Control my font preferences with ELisp.

;;; Code:

;; TODO: `defcustom' font-size.
;; TODO: `defcustom' fonts.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dependencies
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'prelude)
(require 'cycle)
(require 'maybe)
(require 'cl-lib)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: Troubleshoot why "8" appears so large on my desktop.

;; TODO: Consider having a different font size when I'm using my 4K monitor.

(defconst fonts-size "10"
  "My preferred default font-size.")

(defconst fonts-size-step 10
  "The amount (%) by which to increase or decrease a font.")

(defconst fonts-hacker-news-recommendations
  '("APL385 Unicode"
    "Go Mono"
    "Sudo"
    "Monoid"
    "Input Mono Medium" ;; NOTE: Also "Input Mono Thin" is nice.
    )
  "List of fonts optimized for programming I found in a HN article.")

(defconst fonts-whitelist
  (cycle-from-list
   (list-concat
    fonts-hacker-news-recommendations
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

;; TODO: fonts and fonts-whitelist make it difficult to name functions like
;; fonts-set as a generic Emacs function vs choosing a font from the whitelist.

(cl-defun fonts-cycle (&key forward?)
  "Cycle forwards when `FORWARD?' non-nil."
  (let ((font (if forward?
                  (cycle-next! fonts-whitelist)
                (cycle-prev! fonts-whitelist))))
    (message (s-concat "Active font: " font))
    (fonts-set font)))

(defun fonts-next ()
  "Quickly cycle through preferred fonts."
  (interactive)
  (fonts-cycle :forward? t))

(defun fonts-prev ()
  "Quickly cycle through preferred fonts."
  (interactive)
  (fonts-cycle :forward? nil))

(defun fonts-set (font &optional size)
  "Change the font to `FONT' with option integer, SIZE, in pixels."
  (if (maybe-some? size)
      (set-frame-font (string-format "%s %s" font size) nil t)
    (set-frame-font font nil t)))

(defun fonts-whitelist-set (font)
  "Focuses the FONT in the `fonts-whitelist' cycle.
The size of the font is determined by `fonts-size'."
  (prelude-assert (cycle-contains? font fonts-whitelist))
  (cycle-focus! (lambda (x) (equal x font)) fonts-whitelist)
  (fonts-set (fonts-current) fonts-size))

(defun fonts-ivy-select ()
  "Select a font from an ivy prompt."
  (interactive)
  (fonts-whitelist-set
   (ivy-read "Font: " (cycle-to-list fonts-whitelist))))

(defun fonts-print-current ()
  "Message the currently enabled font."
  (interactive)
  (message
   (string-format "[fonts] Current font: \"%s\""
                  (fonts-current))))

(defun fonts-current ()
  "Return the currently enabled font."
  (cycle-current fonts-whitelist))

(defun fonts-increase-size ()
  "Increase font size."
  (interactive)
  (->> (face-attribute 'default :height)
       (+ fonts-size-step)
       (set-face-attribute 'default (selected-frame) :height)))

(defun fonts-decrease-size ()
  "Decrease font size."
  (interactive)
  (->> (face-attribute 'default :height)
       (+ (- fonts-size-step))
       (set-face-attribute 'default (selected-frame) :height)))

(defun fonts-reset-size ()
  "Restore font size to its default value."
  (interactive)
  (fonts-whitelist-set (fonts-current)))

(defun fonts-enable-ligatures ()
  "Call this function to enable ligatures."
  (interactive)
  (let ((alist '((33 . ".\\(?:\\(?:==\\|!!\\)\\|[!=]\\)")
                 (35 . ".\\(?:###\\|##\\|_(\\|[#(?[_{]\\)") ;;
                 (36 . ".\\(?:>\\)")
                 (37 . ".\\(?:\\(?:%%\\)\\|%\\)")
                 (38 . ".\\(?:\\(?:&&\\)\\|&\\)")
                 (42 . ".\\(?:\\(?:\\*\\*/\\)\\|\\(?:\\*[*/]\\)\\|[*/>]\\)") ;;
                 (43 . ".\\(?:\\(?:\\+\\+\\)\\|[+>]\\)")
                 (45 . ".\\(?:\\(?:-[>-]\\|<<\\|>>\\)\\|[<>}~-]\\)")
                 (46 . ".\\(?:\\(?:\\.[.<]\\)\\|[.=-]\\)") ;;
                 (47 . ".\\(?:\\(?:\\*\\*\\|//\\|==\\)\\|[*/=>]\\)")
                 (48 . ".\\(?:x[a-zA-Z]\\)")
                 (58 . ".\\(?:::\\|[:=]\\)")
                 (59 . ".\\(?:;;\\|;\\)")
                 (60 . ".\\(?:\\(?:!--\\)\\|\\(?:~~\\|->\\|\\$>\\|\\*>\\|\\+>\\|--\\|<[<=-]\\|=[<=>]\\||>\\)\\|[*$+~/<=>|-]\\)")
                 (61 . ".\\(?:\\(?:/=\\|:=\\|<<\\|=[=>]\\|>>\\)\\|[<=>~]\\)")
                 (62 . ".\\(?:\\(?:=>\\|>[=>-]\\)\\|[=>-]\\)")
                 (63 . ".\\(?:\\(\\?\\?\\)\\|[:=?]\\)")
                 (91 . ".\\(?:]\\)")
                 (92 . ".\\(?:\\(?:\\\\\\\\\\)\\|\\\\\\)")
                 (94 . ".\\(?:=\\)")
                 (119 . ".\\(?:ww\\)")
                 (123 . ".\\(?:-\\)")
                 (124 . ".\\(?:\\(?:|[=|]\\)\\|[=>|]\\)")
                 (126 . ".\\(?:~>\\|~~\\|[>=@~-]\\)"))))
    (dolist (char-regexp alist)
      (set-char-table-range composition-function-table (car char-regexp)
                            `([,(cdr char-regexp) 0 font-shape-gstring])))))

(provide 'fonts)
;;; fonts.el ends here
