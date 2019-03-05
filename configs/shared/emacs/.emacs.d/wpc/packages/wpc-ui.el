;;; wpc-ui.el --- Any related to the UI/UX goes here -*- lexical-binding: t -*-
;; Author: William Carroll <wpcarro@gmail.com>

;;; Commentary:
;; Hosts font settings, scrolling, color schemes.

;;; Code:

;; increase line height
(setq-default line-spacing 4)

;; change font
(add-to-list 'default-frame-alist '(font . "Source Code Pro-9"))

(defconst wpc/font-size-step 10
  "The amount (%) by which to increase or decrease a font.")

(defun wpc/increase-font ()
  "Increase font size."
  (interactive)
  (->> (face-attribute 'default :height)
       (+ wpc/font-size-step)
       (set-face-attribute 'default (selected-frame) :height)))

(defun wpc/decrease-font ()
  "Decrease font size."
  (interactive)
  (->> (face-attribute 'default :height)
       (+ (- wpc/font-size-step))
       (set-face-attribute 'default (selected-frame) :height)))

(general-define-key "s-j" #'wpc/decrease-font)
(general-define-key "s-k" #'wpc/increase-font)

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
(setq initial-buffer-choice wpc/current-project)

;; transparent Emacs
(set-frame-parameter (selected-frame) 'alpha '(90 . 90))
(setq frame-transparent? t)

(defun wpc/toggle-transparency ()
  "Toggle the frame transparency."
  (interactive)
  (set-frame-parameter (selected-frame) 'alpha '(90 . 90))
  (let ((alpha (if frame-transparent? 100 90)))
    (set-frame-parameter (selected-frame) 'alpha `(,alpha . ,alpha)))
  (setq frame-transparent? (not frame-transparent?)))

(general-define-key "s-u" #'wpc/toggle-transparency)

;; premium Emacs themes
(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-one t)
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

;; reduce noisiness of auto-revert-mode
(setq auto-revert-verbose nil)

;; highlight lines that are over 100 characters long
(use-package whitespace
  :config
  (setq whitespace-line-column wpc/fill-column)
  (setq whitespace-style '(face lines-tail))
  (add-hook 'prog-mode-hook #'whitespace-mode))



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

(provide 'wpc-ui)
;;; wpc-ui.el ends here
