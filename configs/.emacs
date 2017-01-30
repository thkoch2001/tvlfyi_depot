;;; William Carroll's Emacs configuration


;; From `https://github.com/melpa/melpa`
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/")))
(package-initialize)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(command-log-mode-window-size 50)
 '(custom-safe-themes
   (quote
    ("945fe66fbc30a7cbe0ed3e970195a7ee79ee34f49a86bc96d02662ab449b8134" "0f0db69b7a75a7466ef2c093e127a3fe3213ce79b87c95d39ed1eccd6fe69f74" "08b8807d23c290c840bbb14614a83878529359eaba1805618b3be7d61b0b0a32" "9d91458c4ad7c74cf946bd97ad085c0f6a40c370ac0a1cbeb2e3879f15b40553" "6254372d3ffe543979f21c4a4179cd819b808e5dd0f1787e2a2a647f5759c1d1" "8ec2e01474ad56ee33bc0534bdbe7842eea74dccfb576e09f99ef89a705f5501" "5b24babd20e58465e070a8d7850ec573fe30aca66c8383a62a5e7a3588db830b" "eb0a314ac9f75a2bf6ed53563b5d28b563eeba938f8433f6d1db781a47da1366" "3d47d88c86c30150c9a993cc14c808c769dad2d4e9d0388a24fee1fbf61f0971" default)))
 '(evil-shift-width 2)
 '(mouse-wheel-mode nil)
 '(neo-window-fixed-size nil)
 '(neo-window-width 35)
 '(package-selected-packages
   (quote
    (synonyms powerline doom-neotree doom-themes persp-mode use-package helm-projectile persp-projectile perspective projectile with-editor helm-core company helm-ag evil-leader flycheck-mix flycheck-elixir evil-matchit typescript-mode evil-surround erlang elixir-mode golden-ratio flycheck-credo flycheck command-log-mode atom-one-dark-theme exec-path-from-shell clues-theme gotham-theme dracula-theme zenburn-theme fill-column-indicator neotree evil iedit vimrc-mode helm-ispell transpose-frame helm-ack nyan-mode alchemist helm magit dockerfile-mode elm-mode ack)))
 '(popwin-mode t)
 '(popwin:popup-window-height 25)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(hl-line ((t (:inherit nil)))))


;; All-the-fonts
(use-package all-the-icons
  :ensure t)


;; Thesaurus
(use-package synonyms
  :ensure t)


;; Doom Themes
(use-package doom-themes
  :ensure t
  :init
  (load-theme 'doom-molokai t)
  (use-package doom-nlinum))

;; Magit Settings
(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch-popup)))


;; View stream of Emacs commands
(use-package command-log-mode
  :ensure t
  :commands (global-command-log-mode))


;; Flycheck Settings
(use-package flycheck
  :ensure t)


;; Ansi Term
(use-package term
  :bind (:map term-mode-map
         ("M-p" . term-send-up)
         ("M-n" . term-send-down)

         :map term-raw-map
         ("C-h" . evil-window-left)
         ("C-l" . evil-window-right)
         ("C-k" . evil-window-up)
         ("C-j" . evil-window-down)))


;; Projectile Settings
(use-package projectile
  :ensure t
  :commands (projectile-mode))


;; Dired Settings
(use-package dired
  :bind (:map dired-mode-map
        ("c" . find-file)))


;; Evil Settings
(use-package evil
  :ensure t
  :commands (evil-mode local-evil-mode)
  :bind (:map evil-insert-state-map
         ("<escape>" . evil-force-normal-state)

         :map evil-motion-state-map
         ("<return>" . nil)
         ("<tab>" . nil)
         ("SPC" . nil)
         ("M-." . nil)

         :map evil-normal-state-map
         ("<return>" . nil)
         ("M-." . nil)
         ("<tab>" . nil)
         ("C-h" . evil-window-left)
         ("C-l" . evil-window-right)
         ("C-k" . evil-window-up)
         ("C-j" . evil-window-down)
         ("s" . nil)
         ("g c" . comment-or-uncomment-region)
         ("s h" . evil-window-vsplit)
         ("s k" . evil-window-split)
         ("H" . evil-first-non-blank)
         ("L" . evil-end-of-line))
  :init
  (setq evil-emacs-state-cursor '("red" box))
  (setq evil-normal-state-cursor '("green" box))
  (setq evil-visual-state-cursor '("orange" box))
  (setq evil-insert-state-cursor '("red" bar))
  (setq evil-replace-state-cursor '("red" bar))
  (setq evil-operator-state-cursor '("red" hollow))
  (global-evil-matchit-mode t)
  (global-evil-surround-mode t)
  (global-evil-leader-mode t)
  :config
  (define-key evil-normal-state-map "sl" (lambda () (evil-window-vsplit) (evil-window-right)))
  (define-key evil-normal-state-map "sj" (lambda () (evil-window-split) (evil-window-down))))


;; Evil Leader Settings
(use-package evil-leader
  :ensure t
  :commands (global-evil-leader-mode)
  :config
  (evil-leader/set-leader "<SPC>")
  (evil-leader/set-key
    "x" 'helm-M-x
    "<SPC>" 'mode-line-other-buffer
    "n" 'neotree-project-dir
    "t" 'alchemist-project-toggle-file-and-tests
    "f" 'helm-projectile
    "p" 'helm-projectile-ag
    "d" 'dired-jump-other-window
    "h" 'evil-window-left
    "l" 'evil-window-right
    "k" 'evil-window-up
    "j" 'evil-window-down
    "b" 'helm-mini))


;; Evil Match-it
(use-package evil-matchit
  :ensure t
  :commands (global-evil-matchit-mode))


;; Evil Surround
(use-package evil-surround
  :ensure t
  :commands (global-evil-surround-mode))


;; Flycheck Mix Settings
(use-package flycheck-mix
  :ensure t
  :init
  (flycheck-mix-setup))


;; Flycheck Credo Settings
(use-package flycheck-credo
  :ensure t
  :init
  (flycheck-credo-setup))


;; Popwin Settings
(use-package popwin
  :ensure t)

(defun *-popwin-help-mode-off ()
  "Turn `popwin-mode' off for *Help* buffers."

  (when (boundp 'popwin:special-display-config)
    (customize-set-variable 'popwin:special-display-config
                            (delq 'help-mode popwin:special-display-config))))

(defun *-popwin-help-mode-on ()
  "Turn `popwin-mode' on for *Help* buffers."

  (when (boundp 'popwin:special-display-config)
    (customize-set-variable 'popwin:special-display-config
                            (add-to-list 'popwin:special-display-config 'help-mode nil #'eq))))

(add-hook 'helm-minibuffer-set-up-hook #'*-popwin-help-mode-off)
(add-hook 'helm-cleanup-hook #'*-popwin-help-mode-on)

(setq display-buffer-function 'popwin:display-buffer)
(setq helm-split-window-preferred-function 'ignore)
(push '("^\*helm .+\*$" :regexp t) popwin:special-display-config)
(push '("^\*helm-.+\*$" :regexp t) popwin:special-display-config)


;; Alchemist Settings
(use-package alchemist
  :ensure t
  :config
  (setq alchemist-mix-env "prod")
  (setq alchemist-goto-elixir-source-dir "/Users/wpcarro/source_code/elixir")
  (setq alchemist-goto-erlang-source-dir "/Users/wpcarro/source_code/otp/")
  :init
  (linum-mode))

(defun custom-erlang-mode-hook ()
  "Jump to and from Elixir, Erlang, Elixir files."

  (define-key erlang-mode-map (kbd "M-,") 'alchemist-goto-jump-back))

(add-hook 'erlang-mode-hook 'custom-erlang-mode-hook)


;; NeoTree Settings
(use-package neotree
  :ensure t
  :bind (:map neotree-mode-map
         ("j" . next-line)
         ("k" . previous-line)

         ("<return>" . neotree-enter)
         ("<tab>" . neotree-enter)

         ("C-h" . evil-window-left)
         ("C-l" . evil-window-right)
         ("C-k" . evil-window-up)
         ("C-j" . evil-window-down))
  :init
  (hl-line-mode)
  :after
  (patch-neotree-icons))

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


;; Whitespace Settings
(use-package whitespace
  :ensure t
  :commands (whitespace-mode)
  :config
  (setq whitespace-line-column 80)
  (setq whitespace-style '(face lines-tail)))


;; Helm Settings
(use-package helm
  :ensure t
  :commands (helm-mode)
  :bind (
         ("M-x" . helm-M-x)
         ("M-y" . helm-show-kill-ring)
         ("C-x b" . helm-mini)
         :map helm-map
         ("TAB" . helm-execute-persistent-action)
         ("C-z" . helm-select-action)

         :term-raw-map
         ("M-x" . helm-M-x))
  :init
  (setq helm-buffers-fuzzy-matching t)
  (setq helm-recentf-fuzzy-match t)
  (setq helm-semantic-fuzzy-match t)
  (setq helm-imenu-fuzzy-match t)
  (setq helm-locate-fuzzy-match t))


;; Helm Projectile Settings
(use-package helm-projectile
  :ensure t)


;; Company Settings
(use-package company
  :config
  (setq company-idle-delay 0))


(add-hook 'after-init-hook 'evil-mode)
(add-hook 'after-init-hook 'global-whitespace-mode)
(add-hook 'after-init-hook 'global-hl-line-mode)
(add-hook 'after-init-hook 'global-linum-mode)
(add-hook 'after-init-hook 'global-flycheck-mode)
(add-hook 'after-init-hook 'global-company-mode)
(add-hook 'after-init-hook 'projectile-mode)
(add-hook 'after-init-hook 'helm-mode)
(add-hook 'before-save-hook 'delete-trailing-whitespace)


;; Scrolling Settings
(setq scroll-step 1)
(setq scroll-conservatively 10000)


;; Properly configure GUI Emacs to use $PATH values
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))


;; Emacs backup / autosave files
;; (setq-default make-backup-files nil)
(setq backup-directory-alist `(("." . "~/.emacs-tmp")))
(setq auto-save-file-name-transforms `((".*" "~/.emacs-tmp/" t)))


;; Automatically follow symlinks
(setq vc-follow-symlinks t)


;; Commenting / Uncommenting
(global-set-key (kbd "C-x C-;") 'comment-or-uncomment-region)


;; Fullscreen settings
(setq ns-use-native-fullscreen nil)
(global-set-key (kbd "<s-return>") 'toggle-frame-fullscreen)


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
(set-frame-font "Operator Mono 10")


;; Force save buffers
(defun save-buffer-always ()
  "Save the buffer even if it is not modified."
  (interactive)
  (set-buffer-modified-p t)
  (save-buffer))

(global-set-key (kbd "C-x C-s") nil)
(global-set-key (kbd "C-x C-s") 'save-buffer-always)


;; Upgrade all packages
(defun package-upgrade-all ()
  "Upgrade all packages automatically without showing *Packages* buffer."
  (interactive)
  (package-refresh-contents)
  (let (upgrades)
    (cl-flet ((get-version (name where)
                (let ((pkg (cadr (assq name where))))
                  (when pkg
                    (package-desc-version pkg)))))
      (dolist (package (mapcar #'car package-alist))
        (let ((in-archive (get-version package package-archive-contents)))
          (when (and in-archive
                     (version-list-< (get-version package package-alist)
                                     in-archive))
            (push (cadr (assq package package-archive-contents))
                  upgrades)))))
    (if upgrades
        (when (yes-or-no-p
               (message "Upgrade %d package%s (%s)? "
                        (length upgrades)
                        (if (= (length upgrades) 1) "" "s")
                        (mapconcat #'package-desc-full-name upgrades ", ")))
          (save-window-excursion
            (dolist (package-desc upgrades)
              (let ((old-package (cadr (assq (package-desc-name package-desc)
                                             package-alist))))
                (package-install package-desc)
                (package-delete  old-package)))))
      (message "All packages are up to date"))))


;; Add transparency
(set-frame-parameter (selected-frame) 'alpha '(95 . 95))
(add-to-list 'default-frame-alist '(alpha . (95 . 95)))
