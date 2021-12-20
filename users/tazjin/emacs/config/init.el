;;; init.el --- Package bootstrapping. -*- lexical-binding: t; -*-

;; Disable annoying warnings from native compilation.
(setq native-comp-async-report-warnings-errors nil
      warning-suppress-log-types '((comp)))

;; Packages are installed via Nix configuration, this file only
;; initialises the newly loaded packages.

(require 'use-package)
(require 'seq)

;; TODO(tazjin): Figure out what's up with vc.
;;
;; Leaving vc enabled breaks all find-file operations with messages
;; about .git folders being absent, but in random places.
(require 'vc)
(setq vc-handled-backends nil)

(package-initialize)

;; Initialise all packages installed via Nix.
;;
;; TODO: Generate this section in Nix for all packages that do not
;; require special configuration.

;;
;; Packages providing generic functionality.
;;

(use-package ace-window
  :bind (("C-x o" . ace-window))
  :config
  (setq aw-keys '(?f ?j ?d ?k ?s ?l ?a)
        aw-scope 'frame))

(use-package auth-source-pass :config (auth-source-pass-enable))

(use-package avy
  :bind (("M-j" . avy-goto-char)
         ("M-p" . avy-pop-mark)
         ("M-g g" . avy-goto-line)))

(use-package browse-kill-ring)

(use-package company
  :hook ((prog-mode . company-mode))
  :config (setq company-tooltip-align-annotations t))

(use-package counsel
  :after (ivy)
  :config (counsel-mode 1))

(use-package dash)
(use-package gruber-darker-theme)

(use-package eglot
  :custom
  (eglot-autoshutdown t)
  (eglot-send-changes-idle-time 0.3))

(use-package elfeed
  :config
  (setq elfeed-feeds
        '("https://lobste.rs/rss"
          "https://www.anti-spiegel.ru/feed/"
          "https://www.reddit.com/r/lockdownskepticism/.rss"
          "https://www.reddit.com/r/rust/.rss"
          "https://news.ycombinator.com/rss"
          ("https://xkcd.com/atom.xml" media)

          ;; vlogcreations
          ("https://www.youtube.com/feeds/videos.xml?channel_id=UCR0VLWitB2xM4q7tjkoJUPw" media)
          )))

(use-package ht)

(use-package hydra)
(use-package idle-highlight-mode :hook ((prog-mode . idle-highlight-mode)))

(use-package ivy
  :config
  (ivy-mode 1)
  (setq enable-recursive-minibuffers t)
  (setq ivy-use-virtual-buffers t))

(use-package ivy-prescient
  :after (ivy prescient)
  :config
  (ivy-prescient-mode)
  ;; Fixes an issue with how regexes are passed to ripgrep from counsel,
  ;; see raxod502/prescient.el#43
  (setf (alist-get 'counsel-rg ivy-re-builders-alist) #'ivy--regex-plus))

(use-package multiple-cursors)

(use-package notmuch
  :custom
  (notmuch-search-oldest-first nil)
  (notmuch-show-all-tags-list t)
  (notmuch-hello-tag-list-make-query "tag:unread"))

(use-package paredit :hook ((lisp-mode . paredit-mode)
                            (emacs-lisp-mode . paredit-mode)))

(use-package pinentry
  :config
  (setq epa-pinentry-mode 'loopback)
  (pinentry-start))

(use-package prescient
  :after (ivy counsel)
  :config (prescient-persist-mode))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode)
  :custom-face
  (rainbow-delimiters-depth-1-face ((t (:foreground "#2aa198"))))
  (rainbow-delimiters-depth-2-face ((t (:foreground "#b58900"))))
  (rainbow-delimiters-depth-3-face ((t (:foreground "#268bd2"))))
  (rainbow-delimiters-depth-4-face ((t (:foreground "#dc322f"))))
  (rainbow-delimiters-depth-5-face ((t (:foreground "#859900"))))
  (rainbow-delimiters-depth-6-face ((t (:foreground "#268bd2"))))
  (rainbow-delimiters-depth-7-face ((t (:foreground "#cb4b16"))))
  (rainbow-delimiters-depth-8-face ((t (:foreground "#d33682"))))
  (rainbow-delimiters-depth-9-face ((t (:foreground "#839496")))))

(use-package rainbow-mode)
(use-package s)
(use-package string-edit)

(use-package swiper
  :after (counsel ivy)
  :bind (("C-s" . swiper)))

(use-package telephone-line) ;; configuration happens outside of use-package
(use-package term-switcher)
(use-package undo-tree :config (global-undo-tree-mode))
(use-package uuidgen)
(use-package which-key :config (which-key-mode t))

;;
;; Applications in emacs
;;

(use-package magit
  :bind ("C-c g" . magit-status)
  :config (setq magit-repository-directories '(("/home/tazjin/projects" . 2)
                                               ("/home/tazjin" . 1))))

(use-package password-store)
(use-package restclient)

(use-package vterm
  :config (progn
            (setq vterm-shell "fish")
            (setq vterm-exit-functions
                  (lambda (&rest _) (kill-buffer (current-buffer))))
            (setq vterm-kill-buffer-on-exit t))
  :custom-face
  (term-color-black ((t (:background "#282828" :foreground "#282828"))))
  (term-color-blue ((t (:background "#96a6c8" :foreground "#96a6c8"))))
  (term-color-cyan ((t (:background "#1fad83" :foreground "#1fad83"))))
  (term-color-green ((t (:background "#73c936" :foreground "#73c936"))))
  (term-color-magenta ((t (:background "#9e95c7" :foreground "#9e95c7"))))
  (term-color-red ((t (:background "#f43841" :foreground "#f43841"))))
  (term-color-white ((t (:background "#f5f5f5" :foreground "#f5f5f5"))))
  (term-color-yellow ((t (:background "#ffdd33" :foreground "#ffdd33")))))

;; vterm removed the ability to set a custom title generator function
;; via the public API, so this overrides its private title generation
;; function instead
(defun vterm--set-title (title)
  (rename-buffer
   (generate-new-buffer-name
    (format "vterm<%s>"
            (s-trim-left
             (s-chop-prefix "fish" title))))))

;;
;; Packages providing language-specific functionality
;;

(use-package cargo
  :hook ((rust-mode . cargo-minor-mode)
         (cargo-process-mode . visual-line-mode))
  :bind (:map cargo-mode-map ("C-c C-c C-l" . ignore)))

(use-package dockerfile-mode)

(use-package erlang
  :hook ((erlang-mode . (lambda ()
                          ;; Don't indent after '>' while I'm writing
                          (local-set-key ">" 'self-insert-command)))))

(use-package f)

(use-package go-mode
  :bind (:map go-mode-map ("C-c C-r" . recompile))
  :hook ((go-mode . (lambda ()
                      (setq tab-width 2)
                      (setq-local compile-command
                                  (concat "go build " buffer-file-name))))))

(use-package haskell-mode)

(use-package ielm
  :hook ((inferior-emacs-lisp-mode . (lambda ()
                                       (paredit-mode)
                                       (rainbow-delimiters-mode-enable)
                                       (company-mode)))))

(use-package jq-mode
  :config (add-to-list 'auto-mode-alist '("\\.jq\\'" . jq-mode)))

(use-package kotlin-mode
  :hook ((kotlin-mode . (lambda ()
                          (setq indent-line-function #'indent-relative)))))

(use-package lsp-mode)

(use-package markdown-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode)))

(use-package markdown-toc)

(use-package nix-mode
  :hook ((nix-mode . (lambda ()
                       (setq indent-line-function #'nix-indent-line)))))

(use-package nix-util)
(use-package nginx-mode)
(use-package rust-mode)

(use-package sly
  :hook ((sly-mrepl-mode . (lambda ()
                             (paredit-mode)
                             (rainbow-delimiters-mode-enable)
                             (company-mode))))
  :config
  (setq common-lisp-hyperspec-root "file:///home/tazjin/docs/lisp/"))

(use-package telega
  :bind (:map global-map ("s-t" . telega))
  :config
  (telega-mode-line-mode 1)
  (add-hook 'telega-msg-ignore-predicates 'telega-msg-from-blocked-sender-p))

(use-package terraform-mode)
(use-package toml-mode)

(use-package tvl)

(use-package web-mode)
(use-package yaml-mode)
(use-package zoxide)

(use-package passively
  :custom
  (passively-store-state "/persist/tazjin/known-russian-words.el"))

;; Initialise midnight.el, which by default automatically cleans up
;; unused buffers at midnight.
(require 'midnight)

(defgroup tazjin nil
  "Settings related to my configuration")

(defcustom depot-path "/depot"
  "Local path to the depot checkout"
  :group 'tazjin)

;; Configuration changes in `customize` can not actually be persisted
;; to the customise file that Emacs is currently using (since it comes
;; from the Nix store).
;;
;; The way this will work for now is that Emacs will *write*
;; configuration to the file tracked in my repository, while not
;; actually *reading* it from there (unless Emacs is rebuilt).
(setq custom-file (expand-file-name "~/depot/tools/emacs/config/custom.el"))
(load-library "custom")

(defvar home-dir (expand-file-name "~"))

;; Seed RNG
(random t)

;; Load all other Emacs configuration. These configurations are
;; added to `load-path' by Nix.
(mapc 'require '(desktop
                 mail-setup
                 look-and-feel
                 functions
                 settings
                 modes
                 bindings
                 eshell-setup))
(telephone-line-setup)
(ace-window-display-mode)

;; If a local configuration library exists, it should be loaded.
;;
;; This can be provided by calling my Emacs derivation with
;; `withLocalConfig'.
(if-let (local-file (locate-library "local"))
    (load local-file))

(require 'dottime)

(provide 'init)
