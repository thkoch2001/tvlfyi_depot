;;; wpc-ui.el --- Any related to the UI/UX goes here -*- lexical-binding: t -*-
;; Author: William Carroll <wpcarro@gmail.com>

;;; Commentary:
;; Hosts font settings, scrolling, color schemes.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dependencies
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'prelude)
(require 'alist)
(require 'wallpaper)
(require 'fonts)
(require 'themes)
(require 'window-manager)
(require 'device)
(require 'laptop-battery)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; increase line height
(setq-default line-spacing 4)

;; Ensure that buffers update when their contents change on disk.
(global-auto-revert-mode t)

;; smooth scrolling settings
(setq scroll-step 1
      scroll-conservatively 10000)

;; clean up modeline
(use-package diminish
  :config
  (diminish 'emacs-lisp-mode "elisp")
  (diminish 'evil-commentary-mode)
  (diminish 'flycheck-mode)
  (diminish 'auto-revert-mode)
  (diminish 'which-key-mode)
  (diminish 'yas-minor-mode)
  (diminish 'lispyville-mode)
  (diminish 'undo-tree-mode)
  (diminish 'company-mode)
  (diminish 'projectile-mode)
  (diminish 'eldoc-mode)
  ;; This is how to diminish `auto-fill-mode'.
  (diminish 'auto-fill-function)
  (diminish 'counsel-mode)
  (diminish 'ivy-mode))

;; disable startup screen
(setq inhibit-startup-screen t)

;; disable toolbar
(tool-bar-mode -1)

;; TODO: Re-enable `linum-mode' when I figure out why the theming is so ugly.
;; enable line numbers
;; (general-add-hook '(prog-mode-hook
;;                     text-mode-hook
;;                     conf-mode-hook)
;;                   (enable linum-mode))

;; set default buffer for Emacs
(setq initial-buffer-choice constants/current-project)

;; integration with wpgtk (in vendor directory)
;; TODO: Re-enable this when base16-wpgtk are looking better.
;; (require 'wpgtk-theme)

;; base-16 themes to integrate with wpgtk
;; (use-package base16-theme
;;   :config
;;   (require 'wpgtk)
;;   (colorscheme/set 'base16-wpgtk))

;; premium Emacs themes
(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (doom-themes-visual-bell-config)
  (doom-themes-org-config))

;; file browsing
(use-package neotree
  :config
  (global-set-key [f8] #'neotree-toggle))

;; kbd discovery
(use-package which-key
  :config
  (setq which-key-idle-delay 0.25)
  (which-key-mode))

;; completion framework
(use-package ivy
  ;; TODO: Restore behavior where `counsel' is used everywhere.
  :config
  (counsel-mode t)
  (alist/set! #'counsel-M-x "" ivy-initial-inputs-alist)
  ;; prefer using `helpful' variants
  (progn
    (setq counsel-describe-function-function #'helpful-callable)
    (setq counsel-describe-variable-function #'helpful-variable))
  (general-define-key
   :keymaps 'ivy-minibuffer-map
   ;; prev
   "C-k" #'ivy-previous-line
   "<backtab>" #'ivy-previous-line
   ;; next
   "C-j" #'ivy-next-line
   "<tab>" #'ivy-next-line))

(use-package ivy-prescient
  :config
  (ivy-prescient-mode 1)
  (prescient-persist-mode 1))

;; all-the-icons
(use-package all-the-icons
  :config
  ;; Only run this once after installing.
  ;; (all-the-icons-install-fonts)
  )

;; icons for Ivy
(use-package all-the-icons-ivy
  :after (ivy all-the-icons)
  :config
  (all-the-icons-ivy-setup))

;; disable menubar
(menu-bar-mode -1)
(when (string-equal system-type "darwin")
  (setq ns-auto-hide-menu-bar t))

;; reduce noisiness of auto-revert-mode
(setq auto-revert-verbose nil)

;; highlight lines that are over 100 characters long
(use-package whitespace
  :config
  (setq whitespace-line-column constants/fill-column)
  (setq whitespace-style '(face lines-tail))
  (add-hook 'prog-mode-hook #'whitespace-mode))

;; rebalance emacs windows after splits are created
;; (defadvice split-window-below (after rebalance-windows activate)
;;   (balance-windows))
;; (defadvice split-window-right (after rebalance-windows activate)
;;   (balance-windows))
;; (defadvice delete-window (after rebalance-window activate)
;;   (balance-windows))

;; dirname/filename instead of filename<dirname>
(setq uniquify-buffer-name-style 'forward)

;; highlight matching parens, brackets, etc
(show-paren-mode 1)

;; hide the scroll-bars in the GUI
(scroll-bar-mode -1)

;; GUI alerts in emacs
(use-package alert
  :commands (alert)
  :config
  (setq alert-default-style 'notifier))

;; TODO: Should `device/work-laptop?' be a function or a constant that gets set
;; during initialization?
(when (device/work-laptop?)
  (laptop-battery/display))

;; Load a theme
(->> (themes/random)
     themes/set)

(provide 'wpc-ui)
;;; wpc-ui.el ends here
