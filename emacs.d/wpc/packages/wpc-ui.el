;;; ui.el --- Any related to the UI/UX goes here -*- lexical-binding: t -*-
;; Author: William Carroll <wpcarro@gmail.com>

;;; Commentary:
;; Hosts font settings, scrolling, color schemes.

;;; Code:

;; increase line height
(setq-default line-spacing 4)

;; change font
(add-to-list 'default-frame-alist '(font . "Operator Mono-10"))

;; smooth scrolling settings
(setq scroll-step 1
      scroll-conservatively 10000)

;; theme mgt
(use-package cycle-themes
  :after (doom-themes)
  :config
  ;; NOTE: may want to use `defconst' here
  (setq wpc/doom-themes
        (->> (custom-available-themes)
             (-map #'symbol-name)
             (-filter (-partial #'s-starts-with? "doom-"))
             (-map #'intern)))
  (setq cycle-themes-theme-list wpc/doom-themes))

;; clean up modeline
(use-package diminish
  :after (yasnippet ivy which-key)
  :config
  (diminish 'evil-commentary-mode)
  (diminish 'flycheck-mode "Flycheck")
  (diminish 'company-mode "Company")
  (diminish 'auto-revert-mode)
  (diminish 'which-key-mode)
  (diminish 'yas-minor-mode)
  (diminish 'ivy-mode))

;; disable startup screen
(setq inhibit-startup-screen t)

;; disable toolbar
(tool-bar-mode -1)

;; enable line numbers
(general-add-hook '(prog-mode-hook
                    text-mode-hook
                    conf-mode-hook)
                  (enable linum-mode))
;;(add-hook 'after-init-hook (lambda () (set-face-foreground 'linum "#da5468")))

;; set default buffer for Emacs
(setq initial-buffer-choice "~/urbint/grid-front-end")

;; transparent Emacs
(set-frame-parameter (selected-frame) 'alpha '(100 . 100))
(add-to-list 'default-frame-alist '(alpha . (100 . 100)))

;; premium Emacs themes
(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-solarized-light t)
  (doom-themes-visual-bell-config)
  (doom-themes-org-config))

;; kbd discovery
(use-package which-key
  :config
  (setq which-key-idle-delay 0.25)
  (which-key-mode))

;; completion framework
(use-package ivy
  :config
  (ivy-mode t))

;; icons for Ivy
(use-package all-the-icons-ivy
  :after (ivy)
  :config
  (all-the-icons-ivy-setup))

;; disable menubar
(menu-bar-mode -1)
(when (string-equal system-type "darwin")
  (setq ns-auto-hide-menu-bar t))

;; highlight lines that are over 100 characters long
(use-package whitespace
  :config
  (setq whitespace-line-column 100)
  (setq whitespace-style '(face lines-tail)))

;; disable GUI scrollbars
(when (display-graphic-p)
  (scroll-bar-mode -1))

;; rebalance emacs windows after splits are created
(defadvice split-window-below (after rebalance-windows activate)
  (balance-windows))

(defadvice split-window-right (after rebalance-windows activate)
  (balance-windows))

(defadvice delete-window (after rebalance-window activate)
  (balance-windows))

;; dirname/filename instead of filename<dirname>
(setq uniquify-buffer-name-style 'forward)

;; highlight matching parens, brackets, etc
(show-paren-mode 1)

;; GUI alerts in emacs
(use-package alert
  :commands (alert)
  :config
  (setq alert-default-style 'notifier))

;; focus mode
(quelpa '(zen-mode
          :fetcher github
          :repo "aki237/zen-mode"))
(require 'zen-mode)

;; focus mode
(use-package writeroom-mode)

(provide 'wpc-ui)
;;; ui.el ends here
