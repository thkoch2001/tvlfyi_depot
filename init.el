;;; private/grfn/init.el -*- lexical-binding: t; -*-

;; An extra measure to prevent the flash of unstyled mode-line while Emacs is
;; booting up (when Doom is byte-compiled).
(setq-default mode-line-format nil)


;; I've swapped these keys on my keyboard
(setq x-super-keysym 'alt
      x-alt-keysym   'meta)

(setq user-mail-address "root@gws.fyi"
      user-full-name    "Griffin Smith")

(add-hook! doom-big-font-mode
  (setq +doom-modeline-height (if doom-big-font-mode 37 29)))

; (def-package-hook! doom-themes :disable)

(after! rust
  (setq rust-format-on-save t))

; (defconst rust-src-path
;   (-> "/Users/griffin/.cargo/bin/rustc --print sysroot"
;       shell-command-to-string
;       string-trim
;       (concat "/lib/rustlib/src/rust/src")))
;
; (setenv "RUST_SRC_PATH" rust-src-path)
;
; (after! racer
;   (setq racer-rust-src-path rust-src-path))
;
(add-hook! rust-mode
  (flycheck-rust-setup)
  (flycheck-mode)
  (racer-mode)
  (cargo-minor-mode))

(add-hook! elixir-mode
  (require 'flycheck-credo)
  (setq flycheck-elixir-credo-strict t)
  (flycheck-credo-setup)

  (require 'flycheck-mix) (flycheck-mix-setup)

  (require 'flycheck-dialyxir) (flycheck-dialyxir-setup)

  (flycheck-mode))

(setq exec-path (append exec-path '("/Users/griffin/.cargo/bin")))

(after! cargo
  (setq cargo-process--custom-path-to-bin "/Users/griffin/.cargo/bin/cargo"))

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

(defadvice load-theme (after theme-set-overrides activate)
  (dolist (theme-settings theme-overrides)
    (let ((theme (car theme-settings))
          (faces (cadr theme-settings)))
      (if (member theme custom-enabled-themes)
          (dolist (face faces)
            (custom-theme-set-faces theme face))))))

(defcustom theme-overrides nil
  "Association list of override faces to set for different custom themes.")

(defun alist-set (alist-symbol key value)
  "Set VALUE of a KEY in ALIST-SYMBOL."
  (set alist-symbol (cons (list key value) (assq-delete-all key (eval alist-symbol)))))

(alist-set 'theme-overrides 'grfn-solarized-light
           `((font-lock-doc-face ((t (:foreground ,+solarized-s-base1))))
             (font-lock-preprocessor-face ((t (:foreground ,+solarized-red))))
             (font-lock-keyword-face ((t (:foreground ,+solarized-green))))

             (elixir-attribute-face ((t (:foreground ,+solarized-blue))))
             (elixir-atom-face ((t (:foreground ,+solarized-cyan))))
             (linum ((t (:background ,+solarized-s-base2 :foreground ,+solarized-s-base1))))
             (line-number ((t (:background ,+solarized-s-base2 :foreground ,+solarized-s-base1))))

             (haskell-operator-face ((t (:foreground ,+solarized-green))))
             (haskell-keyword-face ((t (:foreground ,+solarized-cyan))))))

(add-to-list 'custom-theme-load-path "~/.doom.d/themes")
(load-theme 'grfn-solarized-light t)

(defface haskell-import-face `((t (:foreground ,+solarized-magenta))) "")

(setq doom-theme 'grfn-solarized-light)
; (setq doom-theme 'doom-solarized-light)

(add-hook! doom-post-init
  (set-face-attribute 'bold nil :weight 'ultra-light)
  (set-face-bold-p 'bold nil))

(defun rx-words (&rest words)
  (rx-to-string
   `(and symbol-start (group (or ,@words)) symbol-end)))

(font-lock-add-keywords
 'elixir-mode
 `((,(rx-words "def"
               "defp"
               "test"
               "describe"
               "property"
               "defrecord"
               "defmodule"
               "defstruct"
               "defdelegate"
               "defprotocol"
               "defimpl"
               "use"
               "import"
               "alias"
               "require"
               "assert"
               "refute"
               "assert_raise")
    .
    'font-lock-preprocessor-face)))

(font-lock-add-keywords
 'elixir-mode
 `((,(rx-words "def"
               "defp"
               "test"
               "describe"
               "property"
               "defrecord"
               "defmodule"
               "defstruct"
               "defdelegate"
               "use"
               "import"
               "alias"
               "require"
               "assert"
               "refute"
               "assert_raise")
    .
    'font-lock-preprocessor-face)))

(font-lock-add-keywords
 'haskell-mode
 `((,(rx-words "import") . 'haskell-import-face)))

;; (font-lock-add-keywords
;;  'haskell-mode
;;  `((,(rx "-- |") . 'haskell-keyword-face)))

;;; * Column Marker
(defun sanityinc/fci-enabled-p () (symbol-value 'fci-mode))

(defvar sanityinc/fci-mode-suppressed nil)
(make-variable-buffer-local 'sanityinc/fci-mode-suppressed)

(defadvice popup-create (before suppress-fci-mode activate)
  "Suspend fci-mode while popups are visible"
  (let ((fci-enabled (sanityinc/fci-enabled-p)))
    (when fci-enabled
      (setq sanityinc/fci-mode-suppressed fci-enabled)
      (turn-off-fci-mode))))

(defadvice popup-delete (after restore-fci-mode activate)
  "Restore fci-mode when all popups have closed"
  (when (and sanityinc/fci-mode-suppressed
             (null popup-instances))
    (setq sanityinc/fci-mode-suppressed nil)
    (turn-on-fci-mode)))


;; https://github.com/alpaker/Fill-Column-Indicator/issues/67#issuecomment-195611974
(after! fill-column-indicator
  (add-hook 'prog-mode-hook #'fci-mode)
  (defvar eos/fci-disabled nil)
  (make-variable-buffer-local 'eos/fci-disabled)

  ;; Add a hook that disables fci if enabled when the window changes and it
  ;; isn't wide enough to display it.
  (defun eos/maybe-disable-fci ()
    (interactive)
    ;; Disable FCI if necessary
    (when (and fci-mode
               (< (window-width) (or fci-rule-column fill-column)))
      (fci-mode -1)
      (setq-local eos/fci-disabled t))
    ;; Enable FCI if necessary
    (when (and eos/fci-disabled
               (eq fci-mode nil)
               (> (window-width) (or fci-rule-column fill-column)))
      (fci-mode 1)
      (setq-local eos/fci-disabled nil)))

  (defun eos/add-fci-disabling-hook ()
    (interactive)
    (add-hook 'window-configuration-change-hook
              #'eos/maybe-disable-fci))

  (add-hook 'prog-mode-hook #'eos/add-fci-disabling-hook))

; (require 'haskell-prettify)

;; (add-hook 'haskell-mode-hook #'haskell-prettify-enable)
