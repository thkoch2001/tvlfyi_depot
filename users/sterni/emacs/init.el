;; Set default font and fallback font via set-fontset-font
;; TODO(sterni): Investigate why ZWJ sequences aren't shaped properly
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
(setq-default indent-tabs-mode nil)
(setq tab-width 2
      css-indent-offset tab-width)

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

;; /tmp is a tmpfs, but we may want to recover from power loss
(custom-set-variables
 `(temporary-file-directory ,(concat (getenv "HOME") "/.emacs/tmp")))

(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq undo-tree-history-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq backup-by-copying t)
(setq create-lockfiles nil)

;; save history
(savehist-mode)
(setq savehist-additional-variables '(search-ring regexp-search-ring magit-cl-history))

;; buffers

;; performance migitations
(global-so-long-mode)

;; unique component should come first for better completion
(setq uniquify-buffer-name-style 'forward)

;; completions
(ido-mode 1)
(setq ido-enable-flex-matching t)
(ido-everywhere)
(fido-mode)

;; Display column numbers
(column-number-mode t)
(setq-default fill-column 80)
(setq display-fill-column-indicator-column t)
(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)

;; whitespace
(setq whitespace-style '(face trailing tabs)
      whitespace-line-column fill-column)
(add-hook 'prog-mode-hook #'whitespace-mode)
(setq-default indicate-empty-lines t)
(setq-default indicate-buffer-boundaries 'left)
(setq sentence-end-double-space nil)

;;; Configure built in modes

;; Perl
(setq perl-indent-level 2)
(setq perl-continued-statement-offset 0)
(setq perl-continued-brace-offset 0)

;; org mode

(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)

(let ((org-folder (concat (getenv "HOME") "/files/sync/org")))
  (setq org-agenda-files (directory-files-recursively org-folder "\\.org$")
        org-default-notes-file (concat org-folder "/inbox.org")
        initial-buffer-choice org-default-notes-file))

;;; Configure packages
(require 'use-package)

(package-initialize)

(use-package undo-tree
  :config
  (global-undo-tree-mode)
  (setq undo-tree-auto-save-history t))

(use-package magit
  :after evil
  :config
  ; reset (buffer-local) fill-column value to emacs' default
  ; gerrit doesn't like 80 column commit messages…
  (add-hook 'git-commit-mode-hook (lambda () (setq fill-column 72)))
  (evil-define-key 'normal 'global (kbd "<leader>gr") 'magit-status))
(use-package tvl
  :after magit
  :custom tvl-depot-path (concat (getenv "HOME") "/src/depot"))

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
  (evil-define-key 'normal 'global (kbd "<leader>bl") 'list-buffers)
  ;; window management: C-w hjkl is annoying in neo
  (define-key evil-window-map (kbd "<left>") 'evil-window-left)
  (define-key evil-window-map (kbd "<right>") 'evil-window-right)
  (define-key evil-window-map (kbd "<up>") 'evil-window-up)
  (define-key evil-window-map (kbd "<down>") 'evil-window-down)
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
  ;; org-mode
  (evil-define-key 'normal 'global (kbd "<leader>oa") 'org-agenda)
  (evil-define-key 'normal 'global (kbd "<leader>oc") 'org-capture)
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

;; parens
(use-package rainbow-delimiters
  :hook ((prog-mode . rainbow-delimiters-mode)))

(setq show-paren-delay 0)
(show-paren-mode)

(use-package paredit
  :hook ((emacs-lisp-mode . paredit-mode)
         (lisp-mode . paredit-mode)
         (ielm-mode . paredit-mode)
         (lisp-interaction-mode . paredit-mode)))

(use-package nix-mode :mode "\\.nix\\'")
(use-package nix-drv-mode :mode "\\.drv\\'")

(use-package direnv
  :config (direnv-mode))

(use-package haskell-mode)
(use-package lsp-mode
  :hook ((haskell-mode . lsp-deferred))
  :commands (lsp lsp-deferred))
(use-package lsp-haskell)

(use-package urweb-mode)
(use-package bqn-mode
  :mode "\\.bqn\\'"
  :custom bqn-mode-map-prefix "C-s-") ; probably rather using C-\
(use-package yaml-mode)
(use-package dockerfile-mode)
(use-package jq-mode
  :config (add-to-list 'auto-mode-alist '("\\.jq\\'" . jq-mode)))
(use-package rust-mode)
(use-package sly
  :after evil
  :hook ((sly-mrepl-mode . (lambda ()
                             (enable-paredit-mode)
                             (rainbow-delimiters-mode-enable))))
  :config
  (evil-define-key '(normal insert) sly-mrepl-mode-map (kbd "C-r") 'isearch-backward))

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
(use-package languagetool
  :after evil
  :custom
  languagetool-java-arguments '("-Dfile.encoding=UTF-8")
  languagetool-default-language "en-GB"
  languagetool-mother-tongue "de-DE"
  :config
  (evil-define-key 'normal 'global (kbd "<leader>ll") 'languagetool-check)
  (evil-define-key 'normal 'global (kbd "<leader>lc") 'languagetool-correct-at-point)
  (evil-define-key 'normal 'global (kbd "<leader>ls") 'languagetool-set-language)
  (evil-define-key 'normal 'global (kbd "<leader>lr") 'languagetool-clear-buffer))

(unless (server-running-p)
  (server-start))

(require 'subscriptions)
(require 'nix-inject)

(provide 'init)
