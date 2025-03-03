;;; wpc-misc.el --- Hosting miscellaneous configuration -*- lexical-binding: t -*-

;; Author: William Carroll <wpcarro@gmail.com>
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1"))

;;; Commentary:
;; This is the home of any configuration that couldn't find a better home.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dependencies
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'project)
(require 'f)
(require 'dash)
(require 'tvl)
(require 'region)
(require 'general)
(require 'constants)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq display-time-string-forms
      '((format-time-string "%H:%M %a %b %d")))
(display-time-mode 1)

;; Remove the boilerplate in the *scratch* buffer
(setq initial-scratch-message "")

;; disable custom variable entries from being written to ~/.emacs.d/init.el
(setq custom-file (f-join user-emacs-directory "custom.el"))
(load custom-file 'noerror)

;; integrate Emacs with X11 clipboard
(customize-set-variable 'select-enable-primary t)
(customize-set-variable 'select-enable-clipboard t)
(customize-set-variable 'evil-visual-update-x-selection-p nil)
(general-def 'insert
  "s-v" #'clipboard-yank
  "C-S-v" #'clipboard-yank)

;; transparently edit compressed files
(auto-compression-mode t)

;; autowrap when over the fill-column
(setq-default auto-fill-function #'do-auto-fill)

;; link to Emacs source code
;; TODO: Update this link.
(setq find-function-C-source-directory
      "~/Dropbox/programming/emacs/src")

;; change emacs prompts from "yes or no" -> "y or n"
(fset 'yes-or-no-p 'y-or-n-p)

;; open photos in Emacs
(auto-image-file-mode 1)

;; disable line-wrapping
(setq-default truncate-lines 1)

;; shell file indentation
(setq sh-basic-offset 2)
(setq sh-indentation 2)

(use-package vterm
  :config
  (general-define-key
   :keymaps '(vterm-mode-map)
   :states '(insert)
   "C-S-v" #'vterm-yank)
  (general-define-key
   :keymaps '(vterm-mode-map)
   :states '(normal)
   "K" #'evil-scroll-line-up
   "J" #'evil-scroll-line-down
   "C-b" #'evil-scroll-page-up
   "C-f" #'evil-scroll-page-down))

;; Use en Emacs buffer as a REST client.
;; For more information: http://emacsrocks.com/e15.html
(use-package restclient)

;; Run `package-lint' before publishing to MELPA.
(use-package package-lint)

;; Parser combinators in Elisp.
(use-package parsec)

;; disable company mode when editing markdown
;; TODO: move this out of wpc-misc.el and into a later file to call
;; `(disable company-mode)'
(use-package markdown-mode
  :config
  ;; TODO: Add assertion that pandoc is installed and it is accessible from
  ;; Emacs.
  (setq markdown-command "pandoc")
  (setq markdown-split-window-direction 'right)
  ;; (add-hook 'markdown-mode-hook #'markdown-live-preview-mode)
  ;; Use mode-specific syntax highlighting for code blocks.
  (setq markdown-fontify-code-blocks-natively t)
  ;; Prevent Emacs from adding a space after the leading 3x-backticks.
  (setq markdown-spaces-after-code-fence 0))

(use-package alert)

(use-package refine)

;; Required by some google-emacs package commands.
(use-package deferred)

;; git integration
(use-package magit
  :config
  (add-hook 'git-commit-setup-hook
            (lambda ()
              (company-mode -1)
              (flyspell-mode 1)))
  (setq magit-display-buffer-function
        #'magit-display-buffer-same-window-except-diff-v1))

(use-package magit-popup)

;; http
(use-package request)

;; TVL depot stuff
(use-package tvl)

;; perl-compatible regular expressions
(use-package pcre2el)

;; alternative to help
(use-package helpful)

;; If called from an existing helpful-mode buffer, reuse that buffer; otherwise,
;; call `pop-to-buffer'.
(setq helpful-switch-buffer-function
      (lambda (buffer-or-name)
        (if (eq major-mode 'helpful-mode)
            (switch-to-buffer buffer-or-name)
          (pop-to-buffer buffer-or-name))))

;; Emacs integration with direnv
(use-package direnv
  :config
  (direnv-mode))

;; Superior Elisp library for working with dates and times.
;; TODO: Put this where my other installations for dash.el, s.el, a.el, and
;; other utility Elisp libraries are located.
(use-package ts)

;; persist history etc b/w Emacs sessions
(setq desktop-save 'if-exists)
(desktop-save-mode 1)
(setq desktop-globals-to-save
      (append '((extended-command-history . 30)
                (file-name-history        . 100)
                (grep-history             . 30)
                (compile-history          . 30)
                (minibuffer-history       . 50)
                (query-replace-history    . 60)
                (read-expression-history  . 60)
                (regexp-history           . 60)
                (regexp-search-ring       . 20)
                (search-ring              . 20)
                (shell-command-history    . 50)
                tags-file-name
                register-alist)))

;; configure ibuffer
(setq ibuffer-default-sorting-mode 'major-mode)

;; Emacs autosave, backup, interlocking files
(setq auto-save-default nil
      make-backup-files nil
      create-lockfiles nil)

;; ensure code wraps at 80 characters by default
(setq-default fill-column 80)

;; render tabs 2x-chars wide
(setq tab-width 2)

(put 'narrow-to-region 'disabled nil)

;; trim whitespace on save
(add-hook 'before-save-hook #'delete-trailing-whitespace)

;; call `git secret hide` after saving secrets.json
(add-hook 'after-save-hook
          (lambda ()
            (when (f-equal? (buffer-file-name)
                            (f-join tvl-depot-path
                                    "users"
                                    "wpcarro"
                                    "secrets.json"))
              (shell-command "git secret hide"))))

;; use tabs instead of spaces
(setq-default indent-tabs-mode nil)

;; prefer shorter tab-widths (e.g. writing Go code)
(setq-default tab-width 2)

;; automatically follow symlinks
(setq vc-follow-symlinks t)

;; fullscreen settings
(setq ns-use-native-fullscreen nil)

(use-package yasnippet
  :config
  (unless constants-ci?
    (setq yas-snippet-dirs (list (f-join user-emacs-directory "snippets")))
    (yas-global-mode 1)))

(use-package projectile
  :config
  (projectile-mode t))

;; TODO(wpcarro): Consider replacing this with a TVL version if it exists.
(defun wpc-misc--depot-find (dir)
  "Find the default.nix nearest to DIR."
  ;; I use 'vc only at the root of my monorepo because 'transient doesn't use my
  ;; .gitignore, which slows things down. Ideally, I could write a version that
  ;; behaves like 'transient but also respects my monorepo's .gitignore and any
  ;; ancestor .gitignore files.
  (if (f-equal? tvl-depot-path dir)
      (cons 'vc dir)
    (when (f-ancestor-of? tvl-depot-path dir)
      (if (f-exists? (f-join dir "default.nix"))
          (cons 'transient dir)
        (wpc-misc--depot-find (f-parent dir))))))

(add-to-list 'project-find-functions #'wpc-misc--depot-find)

(defun wpc-misc-pkill (name)
  "Call the pkill executable using NAME as its argument."
  (interactive "sProcess name: ")
  (call-process "pkill" nil nil nil name))

(use-package deadgrep
  :config
  (general-define-key
   :keymaps 'deadgrep-mode-map
   :states 'normal
   "o" #'deadgrep-visit-result-other-window)
  (setq-default deadgrep--context '(0 . 3))
  (defun wpc-misc-deadgrep-region ()
    "Run a ripgrep search on the active region."
    (interactive)
    (deadgrep (region-to-string)))
  (defun wpc-misc-deadgrep-dwim ()
    "If a region is active, use that as the search, otherwise don't."
    (interactive)
    (with-current-buffer (current-buffer)
      (if (region-active-p)
          (setq deadgrep--additional-flags '("--multiline"))
          (wpc-misc-deadgrep-region)
        (call-interactively #'deadgrep))))
  (advice-add 'deadgrep--arguments
              :filter-return
              (lambda (args)
                (push "--hidden" args)
                (push "--follow" args))))

;; TODO: Do I need this when I have swiper?
(use-package counsel)

(use-package counsel-projectile)

;; search Google, Stackoverflow from within Emacs
(use-package engine-mode
  :config
  (defengine google
    "http://www.google.com/search?ie=utf-8&oe=utf-8&q=%s"
    :keybinding "g")
  (defengine stack-overflow
    "https://stackoverflow.com/search?q=%s"
    :keybinding "s"))

;; EGlot (another LSP client)
(use-package eglot)

;; Microsoft's Debug Adapter Protocol (DAP)
(use-package dap-mode
  :after lsp-mode
  :config
  (dap-mode 1)
  (dap-ui-mode 1))

;; Microsoft's Language Server Protocol (LSP)
(use-package lsp-ui
  :config
  (add-hook 'lsp-mode-hook #'lsp-ui-mode))

;; Wilfred/suggest.el - Tool for discovering functions basesd on declaring your
;; desired inputs and outputs.
(use-package suggest)

;; Malabarba/paradox - Enhances the `list-packages' view.
(use-package paradox
  :config
  (paradox-enable))

;; render emojis in Emacs 🕺
(use-package emojify
  :config
  (add-hook 'after-init-hook #'global-emojify-mode)
  ;; Disable the default styles of:
  ;; - ascii  :P (When this is enabled, the vim command, :x, renders as 😶)
  ;; - github :smile:
  (setq emojify-emoji-styles '(unicode)))

;; Always auto-close parantheses and other pairs
(electric-pair-mode)

;; Start the Emacs server
(when (not (server-running-p))
  (server-start))

(provide 'wpc-misc)
;;; wpc-misc.el ends here
