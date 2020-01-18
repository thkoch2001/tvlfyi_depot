;;; themes.el --- Functions for working with my themes. -*- lexical-binding: t -*-
;; Author: William Carroll <wpcarro@gmail.com>

;;; Commentary:


;; Because I couldn't get cycle-themes to work, I'm writing my own version.
;;
;; Terminology:
;; - colorscheme: determines the colors used by syntax highlighting and other
;;   Emacs UI elements.
;; - theme: Structural representation of a "theme" that includes colorscheme
;;   (see above), font, wallpaper.  "theme" is a superset of "colorscheme".
;;
;; Wishlist:
;; - TODO: Find a way to update the terminal (e.g. terminator) theme.
;; - TODO: Ensure terminal font is updated when Emacs font changes.
;; - TODO: Support a light theme.
;; - TODO: Support Rick & Morty theme.
;; - TODO: Support retro/arcade/80s theme.

;;; Code:

;; Dependencies
(require 'prelude)
(require 'alist)
(require 'symbol)
(require 'f)
(require 'wallpaper)
(require 'fonts)
(require 'cycle)
(require 'symbol)
(require 'random)
(require 'colorscheme)
(require 'dotted)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Library
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The theme struct couples a font, a wallpaper, and a colorschemes.
(cl-defstruct theme
  font
  wallpaper
  colorscheme)

(defvar themes/current nil
  "Store the name of the currently enabled theme.")

(defconst themes/themes
  (list (dotted/new
         "Forest"
         (make-theme
          :font "Operator Mono Light"
          :wallpaper "forest_8k.jpg"
          :colorscheme 'doom-peacock))
        (dotted/new
         "Geometry"
         (make-theme
          :font "Input Mono Medium"
          :wallpaper "geometric_4k.jpg"
          :colorscheme 'doom-molokai))
        (dotted/new
         "Ice"
         (make-theme
          :font "Go Mono"
          :wallpaper "construction_paper_iceberg_4k.jpg"
          :colorscheme 'doom-dracula))
        (dotted/new
         "Lego Manhattan"
         (make-theme
          :font "Input Mono Medium"
          :wallpaper "lego_manhattan.jpg"
          :colorscheme 'base16-atelier-sulphurpool))
        (dotted/new
         "Shapely Patterns"
         (make-theme
          :font "Operator Mono Light"
          :wallpaper "geometric_dark_4k.jpg"
          :colorscheme 'doom-vibrant))
        ;; TODO: Support setting backgrounds as solid colors.
        (dotted/new
         "Gruvbox"
         (make-theme
          :font "JetBrainsMono"
          :wallpaper "geometric_dark_4k.jpg"
          :colorscheme 'doom-gruvbox))
        (dotted/new
         "Solarized Light"
         (make-theme
          :font "JetBrainsMono"
          :wallpaper "solarized_light_thinkpad.jpg"
          :colorscheme 'doom-solarized-light))
        (dotted/new
         "Lightness"
         (make-theme
          :font "Input Mono Medium"
          :wallpaper "construction_paper_iceberg_4k.jpg"
          :colorscheme 'doom-one-light))
        (dotted/new
         "Edison Lightbulb"
         (make-theme
          :font "Mononoki Medium"
          :wallpaper "lightbulb_4k.jpg"
          :colorscheme 'base16-atelier-cave))
        (dotted/new
         "Wall-E"
         (make-theme
          :font "Input Mono Medium"
          :wallpaper "walle_4k.jpg"
          :colorscheme 'doom-material))
        (dotted/new
         "Galaxy"
         (make-theme
          :font "Source Code Pro"
          :wallpaper "galaxy_4k.jpg"
          :colorscheme 'doom-moonlight))
        (dotted/new
         "Underwater"
         (make-theme
          :font "Go Mono"
          ;; TODO: Change this wallpaper to an oceanic scene.
          :wallpaper "galaxy_4k.jpg"
          :colorscheme 'doom-solarized-dark))
        (dotted/new
         "Fantasy Tree"
         (make-theme
          :font "Go Mono"
          :wallpaper "fantasy_tree_4k.jpg"
          :colorscheme 'doom-outrun-electric)))
  "Predefined themes to suit my whims.")

;; TODO: Choose between plural and singular names for Elisp modules.  For
;; example, why have themes.el and colorscheme.el.  I think singular is
;; preferable.
;; TODO: Decide between "message", "show", "print", "inspect" for naming
;; commands that output human-readable information to the "*Messages*" buffer.
;; TODO: Is there a idiomatic CL/Elisp way to print struct information?
(defun themes/print (name)
  "Print a human-readable description of theme named NAME."
  (let* ((theme (alist/get name themes/themes))
         (f (theme-font theme))
         (w (theme-wallpaper theme))
         (c (theme-colorscheme theme)))
    (message (string/format
              "[themes] Name: %s. Font: %s. Wallpaper: %s. Colorscheme: %s"
              name f w c))))

;; TODO: Make this into a proper test.
(defun themes/debug ()
  "Print a human-readable description of theme named NAME."
  (interactive)
  (let ((theme (alist/get themes/current themes/themes)))
    (prelude/assert (equal (theme-font theme)
                           (fonts/current)))
    (prelude/assert (equal (theme-wallpaper theme)
                           (f-filename (wallpaper/current))))
    (prelude/assert (equal (theme-colorscheme theme)
                           (colorscheme/current)))
    (message "[themes] Debug couldn't find any inconsistencies. All good!")))

;; TODO: Assert that all of the dependencies exist before attempting to load
;; theme.
;; TODO: Provide a friendlier way to define themes.
(defun themes/ivy-select ()
  "Use ivy to interactively load a theme."
  (interactive)
  (let* ((name (ivy-read "Theme: " (alist/keys themes/themes))))
    (message (string/format "name: %s" name))
    (themes/set name)))

(defun themes/load (theme)
  "Load the struct, THEME."
  (colorscheme/disable-all)
  (let* ((font (theme-font theme))
         (wallpaper (theme-wallpaper theme))
         (colorscheme (theme-colorscheme theme)))
    (fonts/whitelist-set font)
    (wallpaper/whitelist-set (f-join wallpaper/path-to-dir wallpaper))
    (colorscheme/whitelist-set colorscheme)))

(defun themes/set (name)
  "Set the currently enabled theme to the theme named NAME.
NAME needs to a key defined in `themes/themes'."
  (prelude/assert (alist/has-key? name themes/themes))
  (themes/load (alist/get name themes/themes))
  (setq themes/current name))

(defun themes/print-current ()
  "Print the currently enabled theme."
  (interactive)
  (themes/print themes/current))

(defun themes/random ()
  "Return the name of a randomly selected theme in `themes/themes'."
  (->> themes/themes
       alist/keys
       random/choice))

(provide 'themes)
;;; themes.el ends here
