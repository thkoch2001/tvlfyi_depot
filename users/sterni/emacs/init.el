;; Set default font and fallback font via set-fontset-font
;; TODO(sterni): Investigate non-emoji representation of some glyphs
(let ((mono-font "Bitstream Vera Sans Mono-12")
      (emoji-font "Noto Color Emoji-12"))
  (setq default-frame-alist `((font . ,mono-font)))
  (set-frame-font mono-font t t)
  (set-fontset-font t nil emoji-font))

(setq inhibit-startup-message t
      display-time-24hr-format t
      select-enable-clipboard t)

;; Reload files
(global-auto-revert-mode 1)

;; Indent
(set-default 'indent-tabs-mode nil)
(setq tab-width 2)

;; UTF-8
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Disable unnecessary GUI elements
(scroll-bar-mode 0)
(menu-bar-mode 0)
(tool-bar-mode 0)

(add-hook 'after-make-frame-functions
          (lambda (frame) (scroll-bar-mode 0)))

;; don't center on cursor when scrolling
(setq scroll-conservatively 1)

;; type less
(defalias 'yes-or-no-p 'y-or-n-p)

;; Extra settings when graphical session
(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (mouse-wheel-mode t)
  (blink-cursor-mode -1))

;; TODO(sterni): prevent some remaining backup files
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))

;; buffers
;; unique component should come first for better completion
(setq uniquify-buffer-name-style 'forward)

;; Display column numbers
(column-number-mode t)
(setq-default fill-column 80)
(setq display-fill-column-indicator-column t)
(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)

;; whitespace
(setq whitespace-style '(face trailing tabs)
      whitespace-line-column fill-column)
(add-hook 'prog-mode-hook #'whitespace-mode)

;;; Configure built in modes

;; Perl
(setq perl-indent-level 2)
(setq perl-continued-statement-offset 0)
(setq perl-continued-brace-offset 0)

;;; Configure packages
(require 'use-package)

(package-initialize)

(use-package undo-tree
  :config
  (global-undo-tree-mode))

(use-package magit
  :after evil
  :config
  ; reset (buffer-local) fill-column value to emacs' default
  ; gerrit doesn't like 80 column commit messages…
  (add-hook 'git-commit-mode-hook (lambda () (setq fill-column 72)))
  (evil-define-key 'normal 'global (kbd "<leader>gr") 'magit-status))
(use-package tvl :after magit)

(setq ediff-split-window-function 'split-window-horizontally)

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-shift-width 2)
  (setq evil-split-window-below t)
  (setq evil-split-window-right t)
  (setq evil-undo-system 'undo-tree)
  :config
  (evil-mode 1)
  (evil-set-leader 'normal ",") ;; TODO(sterni): space would be nice, but…
  (evil-set-leader 'visual ",")
  ;; buffer management
  (evil-define-key 'normal 'global (kbd "<leader>bk") 'kill-buffer)
  (evil-define-key 'normal 'global (kbd "<leader>bb") 'switch-to-buffer)
  ;; window management
  (evil-define-key 'normal 'global (kbd "<leader>wk") 'delete-window)
  (evil-define-key 'normal 'global (kbd "<leader>wo") 'delete-other-window)
  (evil-define-key 'normal 'global (kbd "<leader>wh") 'split-window-below)
  (evil-define-key 'normal 'global (kbd "<leader>wv") 'split-window-right)
  (evil-define-key 'normal 'global (kbd "<leader>ww") 'other-window)
  ;; emacs
  (evil-define-key 'visual 'global (kbd "<leader>ee") 'eval-region)
  (evil-define-key 'normal 'global (kbd "<leader>ee") 'eval-last-sexp)
  (evil-define-key 'normal 'global (kbd "<leader>ep") 'eval-print-last-sexp)
  (evil-define-key 'normal 'global (kbd "<leader>eh") 'help)
  ;; modify what is displayed
  (evil-define-key 'normal 'global (kbd "<leader>dw")
    (lambda ()
      (interactive)
      (whitespace-mode 'toggle)
      (display-fill-column-indicator-mode 'toggle)))
  ;; elfeed bindings for evil (can't use-package elfeed apparently)
  (evil-define-key 'normal 'global (kbd "<leader>ff") 'elfeed)
  (evil-define-key '(normal visual) elfeed-search-mode-map
    (kbd "o") 'elfeed-search-browse-url
    (kbd "r") 'elfeed-search-untag-all-unread
    (kbd "u") 'elfeed-search-tag-all-unread
    (kbd "<leader>ff") 'elfeed-search-fetch
    (kbd "<leader>fc") 'elfeed-db-compact
    (kbd "<leader>fr") 'elfeed-search-update--force))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package nix-mode :mode "\\.nix\\'")
(use-package nix-drv-mode :mode "\\.drv\\'")

(use-package haskell-mode)
(use-package urweb-mode)
(use-package bqn-mode
  :mode "\\.bqn\\'"
  :custom bqn-mode-map-prefix "C-s-") ; probably rather using C-\
(use-package yaml-mode)
(use-package dockerfile-mode)
(use-package rust-mode)
(use-package sly
  :after evil
  :hook ((sly-mrepl-mode . (lambda () (rainbow-delimiters-mode-enable))))
  :config
  (evil-define-key 'normal sly-mrepl-mode-map (kbd "C-r") 'isearch-backward))

(use-package ada-mode)

(use-package rainbow-mode)
(use-package hl-todo
  :hook ((prog-mode . hl-todo-mode))
  :config
  (setq hl-todo-keyword-faces
        '(("TODO"  . "#FF0000")
          ("FIXME" . "#FF0000")
          ("HACK"  . "#7f7f7f")
          ("XXX"   . "#aa0000"))))

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("\\.md\\'" . markdown-mode)))
(use-package adoc-mode
  :mode (("\\.adoc\\'" . adoc-mode)))

(require 'subscriptions)

(provide 'init)
