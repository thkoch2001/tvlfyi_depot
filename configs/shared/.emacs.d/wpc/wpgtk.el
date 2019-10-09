;; wpgtk.el -- A base16 colorscheme template for wpgtk.

;;; Commentary:

;;; Authors:
;; Template: William Carroll <wpcarro@gmail.com>

;;; Code:

(require 'base16-theme)
(require 'colorscheme)

(defvar base16-wpgtk-colors
  '(:base00 "#31213f"
    :base01 "#E29B61"
    :base02 "#E8C35F"
    :base03 "#565B87"
    :base04 "#A56785"
    :base05 "#20A89E"
    :base06 "#3CC2B5"
    :base07 "#8de0e1"
    :base08 "#629c9d"
    :base09 "#E29B61"
    :base0A "#E8C35F"
    :base0B "#565B87"
    :base0C "#A56785"
    :base0D "#20A89E"
    :base0E "#3CC2B5"
    :base0F "#8de0e1")
  "All colors for Base16 wpgtk are defined here.")

;; Define the theme
(deftheme base16-wpgtk)

;; Add all the faces to the theme
(base16-theme-define 'base16-wpgtk base16-wpgtk-colors)

;; Mark the theme as provided
(provide-theme 'base16-wpgtk)

(macros/comment
 (colorscheme/set 'base16-wpgtk))

(provide 'wpgtk)
;;; wpgtk.el ends here
