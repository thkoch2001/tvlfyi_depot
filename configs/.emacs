(package-initialize)

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(mouse-wheel-mode nil)
 '(neo-window-fixed-size nil)
 '(neo-window-width 35)
 '(package-selected-packages
   (quote
    (neotree evil helm-swoop iedit vimrc-mode helm-ispell transpose-frame helm-projectile helm-ack nyan-mode alchemist helm magit dockerfile-mode elixir-mode elm-mode ack))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(hl-line ((t (:background "gray7")))))

;; Colorscheme
(load-theme 'wombat)

;; Emacs backup files
(setq-default make-backup-files nil)

;; Automatically follow symlinks
(setq vc-follow-symlinks t)

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


;; Evil Settings
(require 'evil)


;; NeoTree Settings
(require 'neotree)

(defun neotree-project-dir ()
  "Open NeoTree using the git root."
  (interactive)
  (let ((project-dir (projectile-project-root))
        (file-name (buffer-file-name)))
    (neotree-toggle)
    (if project-dir
        (if (neo-global--window-exists-p)
            (progn
              (neotree-dir project-dir)
              (neotree-find file-name)))
      (message "Could not find git project root."))))

(global-set-key (kbd "<f8>") 'neotree-project-dir)

(add-hook 'neotree-mode-hook (lambda () (bootstrap-evil-mode) (hl-line-mode)) )


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


;; Personalized Evil-mode settings
(defun bootstrap-evil-mode()
  "Custom evil-mode settings. This disables Emacs key-bindings found in 
`global-map` when inside Vim's `normal` mode. It disables Vim key-bindings
when in Vim's `insert` mode, favoring native Emacs bindings instead."
  (interactive)
  (evil-local-mode)

  ;; Toggle off Emacs bindings when in Vim `normal` mode except:
  ;;   * `M-x`
  ;; (setcdr global-map nil)
  ;; (define-key global-map (kbd "M-x") 'helm-M-x)

  ;; unbind <SPC> and <CR> in normal mode since they're hardly used
  (define-key evil-motion-state-map (kbd "RET") nil)
  (define-key evil-motion-state-map (kbd "SPC") nil)

  ;; use 'helm-swoop for interactive search
  (define-key evil-motion-state-map (kbd "/") 'helm-swoop)

  (define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
  (define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)
  (define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
  (define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)

  (define-key evil-normal-state-map (kbd "M-l") (lambda () (interactive) (evil-window-vsplit) (evil-window-right 1) ))
  (define-key evil-normal-state-map (kbd "M-h") (lambda () (interactive) (evil-window-vsplit) ))
  (define-key evil-normal-state-map (kbd "M-j") (lambda () (interactive) (evil-window-split) (evil-window-down 1) ))
  (define-key evil-normal-state-map (kbd "M-k") (lambda () (interactive) (evil-window-split) ))
  
  ;; Toggle off Vim bindings when in Vim `insert` mode except:
  ;;   * `<escape>` <ESC>
  ;;   * `j k` <ESC>
  ;;   * `j j` "j"
  (setcdr evil-insert-state-map nil)
  (define-key evil-insert-state-map (kbd "<escape>") 'evil-force-normal-state)
  (define-key evil-insert-state-map (kbd "j k") 'evil-force-normal-state)
  (define-key evil-insert-state-map (kbd "jj") (lambda () (interactive) (insert "j")))
  
  (define-key evil-normal-state-map (kbd "H") 'evil-first-non-blank)
  (define-key evil-normal-state-map (kbd "L") 'evil-end-of-line))


;; Line Numbers in margin for source code mode
(add-hook 'prog-mode-hook 'linum-mode)
(add-hook 'prog-mode-hook 'bootstrap-evil-mode)

;; Add transparency
(set-frame-parameter (selected-frame) 'alpha '(90 . 50))
(add-to-list 'default-frame-alist '(alpha . (90 . 50)))

;; Full-screen as Command <CR>
(global-set-key (kbd "<s-return>") 'toggle-frame-fullscreen)
