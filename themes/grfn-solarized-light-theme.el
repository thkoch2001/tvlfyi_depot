(require 'solarized)

;; (defun grfn-solarized-theme ()
;;   (custom-theme-set-faces
;;    theme-name
;;    `(font-lock-doc-face ((,class (:foreground ,s-base1))))
;;    `(font-lock-preprocessor-face ((,class (:foreground ,red))))
;;    `(font-lock-keyword-face ((,class (:foreground ,green))))

;;    `(elixir-attribute-face ((,class (:foreground ,blue))))
;;    `(elixir-atom-face ((,class (:foreground ,cyan))))))

(setq +solarized-s-base03    "#002b36"
      +solarized-s-base02    "#073642"
      ;; emphasized content
      +solarized-s-base01    "#586e75"
      ;; primary content
      +solarized-s-base00    "#657b83"
      +solarized-s-base0     "#839496"
      ;; comments
      +solarized-s-base1     "#93a1a1"
      ;; background highlight light
      +solarized-s-base2     "#eee8d5"
      ;; background light
      +solarized-s-base3     "#fdf6e3"

      ;; Solarized accented colors
      +solarized-yellow    "#b58900"
      +solarized-orange    "#cb4b16"
      +solarized-red       "#dc322f"
      +solarized-magenta   "#d33682"
      +solarized-violet    "#6c71c4"
      +solarized-blue      "#268bd2"
      +solarized-cyan      "#2aa198"
      +solarized-green     "#859900"

      ;; Darker and lighter accented colors
      ;; Only use these in exceptional circumstances!
      +solarized-yellow-d  "#7B6000"
      +solarized-yellow-l  "#DEB542"
      +solarized-orange-d  "#8B2C02"
      +solarized-orange-l  "#F2804F"
      +solarized-red-d     "#990A1B"
      +solarized-red-l     "#FF6E64"
      +solarized-magenta-d "#93115C"
      +solarized-magenta-l "#F771AC"
      +solarized-violet-d  "#3F4D91"
      +solarized-violet-l  "#9EA0E5"
      +solarized-blue-d    "#00629D"
      +solarized-blue-l    "#69B7F0"
      +solarized-cyan-d    "#00736F"
      +solarized-cyan-l    "#69CABF"
      +solarized-green-d   "#546E00"
      +solarized-green-l "#B4C342")


(deftheme grfn-solarized-light "The light variant of Griffin's solarized theme")

(create-solarized-theme
 'light 'grfn-solarized-light
 (lambda ()
   (custom-theme-set-faces
    'grfn-solarized-light
    `(font-lock-doc-face ((t (:foreground ,+solarized-s-base1))))
    `(font-lock-preprocessor-face ((t (:foreground ,+solarized-red))))
    `(font-lock-keyword-face ((t (:foreground ,+solarized-green))))

    `(elixir-attribute-face ((t (:foreground ,+solarized-blue))))
    `(elixir-atom-face ((t (:foreground ,+solarized-cyan))))
    )

   ))

(custom-theme-set-faces
 'grfn-solarized-light
 `(font-lock-doc-face ((t (:foreground ,+solarized-s-base1))))
 `(font-lock-preprocessor-face ((t (:foreground ,+solarized-red))))
 `(font-lock-keyword-face ((t (:foreground ,+solarized-green))))

 `(elixir-attribute-face ((t (:foreground ,+solarized-blue))))
 `(elixir-atom-face ((t (:foreground ,+solarized-cyan))))
 )

(provide-theme 'grfn-solarized-light)

