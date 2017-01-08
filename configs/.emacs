(package-initialize)

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("6254372d3ffe543979f21c4a4179cd819b808e5dd0f1787e2a2a647f5759c1d1" "8ec2e01474ad56ee33bc0534bdbe7842eea74dccfb576e09f99ef89a705f5501" "5b24babd20e58465e070a8d7850ec573fe30aca66c8383a62a5e7a3588db830b" "eb0a314ac9f75a2bf6ed53563b5d28b563eeba938f8433f6d1db781a47da1366" "3d47d88c86c30150c9a993cc14c808c769dad2d4e9d0388a24fee1fbf61f0971" default)))
 '(evil-shift-width 2)
 '(mouse-wheel-mode nil)
 '(neo-window-fixed-size nil)
 '(neo-window-width 35)
 '(package-selected-packages
   (quote
    (atom-one-dark-theme exec-path-from-shell clues-theme gotham-theme dracula-theme zenburn-theme fill-column-indicator neotree evil helm-swoop iedit vimrc-mode helm-ispell transpose-frame helm-projectile helm-ack nyan-mode alchemist helm magit dockerfile-mode elm-mode ack))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(hl-line ((t (:inherit nil)))))


;; Colorscheme
(load-theme 'atom-one-dark)


;; Properly configure GUI Emacs to use $PATH values
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))


;; Emacs backup / autosave files
;; (setq-default make-backup-files nil)
(setq backup-inhibited t)
(setq auto-save-default nil)


;; Automatically follow symlinks
(setq vc-follow-symlinks t)


;; Enable autocompletion
(add-hook 'after-init-hook 'global-company-mode)


;; Fullscreen settings
(setq ns-use-native-fullscreen nil)
(global-set-key (kbd "<s-return>") 'toggle-frame-fullscreen)


;; Helm Settings
(require 'helm)
(require 'helm-config)

(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-projectile)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-b") 'helm-buffers-list)
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


;; Add 80 column marker
(require 'whitespace)
(setq whitespace-line-column 80) ;; limit line length
(setq whitespace-style '(face lines-tail))
(add-hook 'prog-mode-hook 'whitespace-mode)


;; Projectile Settings
(projectile-mode t)
(setq projectile-completion-system 'helm)
(helm-projectile-on)
(setq projectile-switch-project-action 'helm-projectile)


;; Elixir Mode
;; Add support for local function invocation highlighting
;; (font-lock-add-keywords 'elixir-mode
;;                         '(("[_a-z]+\\.\\(" . font-lock-variable-name-face)))



;; Alchemist Settings
(require 'alchemist)
(setq alchemist-mix-env "prod")

(setq alchemist-goto-erlang-source-dir "/usr/local/bin/source/")
(setq alchemist-goto-elixir-source-dir "/usr/local/bin/erl")

;; Borrow keybinding from list-mode eval
(define-key global-map (kbd "C-j") nil)
(define-key alchemist-mode-keymap (kbd "C-j") 'alchemist-eval-current-line)

;; Allow Elixir -> Erlang -> Elixir definition jumping
(defun custom-erlang-mode-hook ()
  (define-key erlang-mode-map (kbd "M-,") 'alchemist-goto-jump-back))

(add-hook 'erlang-mode-hook 'custom-erlang-mode-hook)

;; Run tests on file writes
;; (setq alchemist-hooks-test-on-save t)


;; Evil Settings
(require 'evil)

(defun register-evil-keybindings-for-neotree ()
  "Registers keybindings for Evil mode for NeoTree plugin."
  (evil-define-key 'normal neotree-mode-map (kbd "TAB") 'neotree-enter)
  (evil-define-key 'normal neotree-mode-map (kbd "SPC") 'neotree-enter)
  (evil-define-key 'normal neotree-mode-map (kbd "q") 'neotree-hide)
  (evil-define-key 'normal neotree-mode-map (kbd "RET") 'neotree-enter))


;; Display column number alongside row number
(column-number-mode t)

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


;; Window movement
(global-set-key (kbd "C-c w f") 'windmove-right)
(global-set-key (kbd "C-c w b") 'windmove-left)
(global-set-key (kbd "C-c w p") 'windmove-up)
(global-set-key (kbd "C-c w n") 'windmove-down)


;; General Settings
;; Hide the menu-bar
(setq ns-auto-hide-menu-bar t)

;; Native App Settings
(tool-bar-mode -1)

;; Disable GUI scrollbars
(scroll-bar-mode -1)

;; Use spaces instead of tabs
(setq-default indent-tabs-mode nil)

;; Change font settings
(set-face-attribute 'default nil :height 100)
(add-to-list 'default-frame-alist '(font . "Operator Mono"))


;; Personalized Evil-mode settings
(defun bootstrap-evil-mode()
  "Custom evil-mode settings. This disables Emacs key-bindings found in 
`global-map` when inside Vim's `normal` mode. It disables Vim key-bindings
when in Vim's `insert` mode, favoring native Emacs bindings instead."
  (interactive)
  (evil-local-mode t)

  ;; Toggle off Emacs bindings when in Vim `normal` mode except:
  ;;   * `M-x`
  ;; (setcdr global-map nil)
  ;; (define-key global-map (kbd "M-x") 'helm-M-x)

  (define-key evil-normal-state-map (kbd "") nil)

  ;; unbind <SPC> and <CR> in normal mode since they're hardly used
  (define-key evil-motion-state-map (kbd "RET") nil)
  (define-key evil-motion-state-map (kbd "SPC") nil)
  (define-key evil-normal-state-map (kbd "M-.") nil)

  ;; use 'helm-swoop for interactive search
  (define-key evil-motion-state-map (kbd "/") 'helm-swoop)
  (define-key evil-motion-state-map (kbd "?") 'helm-swoop)

  (define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
  (define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)
  (define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
  (define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)

  (define-key evil-normal-state-map (kbd "M-l") (lambda () (interactive) (evil-window-vsplit) (evil-window-right 1) ))
  (define-key evil-normal-state-map (kbd "M-h") (lambda () (interactive) (evil-window-vsplit) ))
  (define-key evil-normal-state-map (kbd "M-j") (lambda () (interactive) (evil-window-split) (evil-window-down 1) ))
  (define-key evil-normal-state-map (kbd "M-k") (lambda () (interactive) (evil-window-split) ))

  ;; Plugin-specific keybindings
  (register-evil-keybindings-for-neotree)
  
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
(set-frame-parameter (selected-frame) 'alpha '(90 . 90))
(add-to-list 'default-frame-alist '(alpha . (90 . 90)))
