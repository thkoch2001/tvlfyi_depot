(package-initialize)

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(custom-set-variables
 '(mouse-wheel-mode nil)
 '(package-selected-packages
   (quote
    (evil helm-swoop iedit vimrc-mode helm-ispell transpose-frame helm-projectile helm-ack nyan-mode alchemist helm magit dockerfile-mode elixir-mode elm-mode ack))))
(custom-set-faces)

;; Colorscheme
(load-theme 'wombat)

;; Emacs backup files
(setq-default make-backup-files nil)

;; NyanCat progress bar
;; (nyan-mode)

;; Enable autocompletion
(add-hook 'after-init-hook 'global-company-mode)

;; Helm Settings
(require 'helm)
(require 'helm-config)

(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-projectile)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-c h o") 'helm-swoop)

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-z")  'helm-select-action)

(setq helm-buffers-fuzzy-matching t
      helm-recentf-fuzzy-match    t)

(setq helm-semantic-fuzzy-match t
      helm-imenu-fuzzy-match    t)

(setq helm-locate-fuzzy-match t)

(helm-mode 1)


;; Projectile Settings
(projectile-mode)
(setq projectile-completion-system 'helm)
(helm-projectile-on)

(setq projectile-switch-project-action 'helm-projectile)


;; Alchemist Settings
(add-hook 'elixir-mode-hook 'alchemist-mode)


;; Buffer scrolling functions
(global-set-key (kbd "M-n") (lambda () (interactive) (scroll-up 1) (next-line 1)))
(global-set-key (kbd "M-p") (lambda () (interactive) (scroll-down 1) (previous-line 1)))


;; Window movement
(global-set-key (kbd "C-c w f") 'windmove-right)
(global-set-key (kbd "C-c w b") 'windmove-left)
(global-set-key (kbd "C-c w p") 'windmove-up)
(global-set-key (kbd "C-c w n") 'windmove-down)


;; General Settings
;; Hide the menu-bar
(setq ns-auto-hide-menu-bar t)

;; Use non-native fullscreen
(setq ns-use-native-fullscreen nil)

;; Native App Settings
(tool-bar-mode -1)

;; Disable GUI scrollbars
(scroll-bar-mode -1)

;; Use spaces instead of tabs
(setq-default indent-tabs-mode nil)

;; Change font settings
(add-to-list 'default-frame-alist '(font . "Hasklig"))

(defun bootstrap-evil-mode()
  "Custom evil-mode boostrapping"
  (interactive)
  (evil-mode)
  (define-key evil-insert-state-map (kbd "j k") 'evil-force-normal-state)
  (define-key evil-normal-state-map (kbd "H") 'evil-beginning-of-line)
  (define-key evil-normal-state-map (kbd "L") 'evil-end-of-line))

;; Line Numbers in margin for source code mode
;; (add-hook 'prog-mode-hook 'linum-mode)
(add-hook 'prog-mode-hook 'bootstrap-evil-mode)

;; Add transparency
(set-frame-parameter (selected-frame) 'alpha '(90 . 50))
(add-to-list 'default-frame-alist '(alpha . (90 . 50)))

;; Full-screen as Command <CR>
(global-set-key (kbd "<s-return>") 'toggle-frame-fullscreen)
