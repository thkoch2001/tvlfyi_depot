(require 'solarized)
(eval-when-compile
  (require 'solarized-palettes))

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

(setq grfn-solarized-faces
      '("Griffin's solarized theme customization"
        (custom-theme-set-faces
         theme-name
         `(font-lock-doc-face ((t (:foreground ,+solarized-s-base1))))
         `(font-lock-preprocessor-face ((t (:foreground ,+solarized-red))))
         `(font-lock-keyword-face ((t (:foreground ,+solarized-green))))

         `(elixir-attribute-face ((t (:foreground ,+solarized-blue))))
         `(elixir-atom-face ((t (:foreground ,+solarized-cyan))))
         `(agda2-highlight-keyword-face ((t (:foreground ,green))))
         `(agda2-highlight-string-face ((t (:foreground ,cyan))))
         `(agda2-highlight-number-face ((t (:foreground ,violet))))
         `(agda2-highlight-symbol-face ((((background ,base3)) (:foreground ,base01))))
         `(agda2-highlight-primitive-type-face ((t (:foreground ,blue))))
         `(agda2-highlight-bound-variable-face ((t nil)))
         `(agda2-highlight-inductive-constructor-face ((t (:foreground ,green))))
         `(agda2-highlight-coinductive-constructor-face ((t (:foreground ,yellow))))
         `(agda2-highlight-datatype-face ((t (:foreground ,blue))))
         `(agda2-highlight-field-face ((t (:foreground ,red))))
         `(agda2-highlight-function-face ((t (:foreground ,blue))))
         `(agda2-highlight-module-face ((t (:foreground ,yellow))))
         `(agda2-highlight-postulate-face ((t (:foreground ,blue))))
         `(agda2-highlight-primitive-face ((t (:foreground ,blue))))
         `(agda2-highlight-record-face ((t (:foreground ,blue))))
         `(agda2-highlight-dotted-face ((t nil)))
         `(agda2-highlight-operator-face ((t nil)))
         `(agda2-highlight-error-face ((t (:foreground ,red :underline t))))
         `(agda2-highlight-unsolved-meta-face ((t (:background ,base2))))
         `(agda2-highlight-unsolved-constraint-face ((t (:background ,base2))))
         `(agda2-highlight-termination-problem-face ((t (:background ,orange :foreground ,base03))))
         `(agda2-highlight-incomplete-pattern-face ((t (:background ,orange :foreground ,base03))))
         `(agda2-highlight-typechecks-face ((t (:background ,cyan :foreground ,base03))))

         `(font-lock-doc-face ((t (:foreground ,+solarized-s-base1))))
         `(font-lock-preprocessor-face ((t (:foreground ,+solarized-red))))
         `(font-lock-keyword-face ((t (:foreground ,+solarized-green :bold nil))))
         `(font-lock-builtin-face ((t (:foreground ,+solarized-s-base01
                                                  :bold t))))

         `(elixir-attribute-face ((t (:foreground ,+solarized-blue))))
         `(elixir-atom-face ((t (:foreground ,+solarized-cyan))))
         `(linum ((t (:background ,+solarized-s-base2 :foreground ,+solarized-s-base1))))
         `(line-number ((t (:background ,+solarized-s-base2 :foreground ,+solarized-s-base1))))

         `(haskell-operator-face ((t (:foreground ,+solarized-green))))
         `(haskell-keyword-face ((t (:foreground ,+solarized-cyan))))

         `(org-drawer ((t (:foreground ,+solarized-s-base1
                                      :bold t)))))))

(solarized-with-color-variables
  'light 'grfn-solarized-light solarized-light-color-palette-alist)

(provide-theme 'grfn-solarized-light)
