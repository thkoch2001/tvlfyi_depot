;;; misc.el --- Hosting miscellaneous configuration -*- lexical-binding: t -*-
;; Author: William Carroll <wpcarro@gmail.com>

;;; Commentary:
;; This is the home of any configuration that couldn't find a better home.

;;; Code:

;; disable custom variable entries from being written to ~/.emacs.d/init.el
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

;; transparently edit compressed files
(auto-compression-mode t)

;; change emacs prompts from "yes or no" -> "y or n"
(fset 'yes-or-no-p 'y-or-n-p)

;; open photos in Emacs
(auto-image-file-mode 1)

;; disable line-wrapping
(setq-default truncate-lines 1)

;; shell file indentation
(setq sh-basic-offset 2)
(setq sh-indentation 2)

;; create file bookmarks
(set-register ?e '(file . "~/.emacs.d/wpc/packages"))
(set-register ?u '(file . "~/urbint"))
(set-register ?d '(file . "~/dotfiles"))
(set-register ?D '(file . "~/Dropbox"))
(set-register ?o '(file . "~/Dropbox/org/"))
(set-register ?c '(file . "~/Dropbox/org/chains.org"))
(set-register ?b '(file . "~/Dropbox/org/backlog.org"))
(set-register ?p '(file . "~/urbint/grid-client"))

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

;; config Emacs to use $PATH values
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :config
  (exec-path-from-shell-initialize))

;; Emacs autosave, backup, interlocking files
(setq auto-save-default nil
      make-backup-files nil
      create-lockfiles nil)

;; ensure code wraps at 80 characters by default
(setq-default fill-column 80)

(put 'narrow-to-region 'disabled nil)

;; trim whitespace on save
(add-hook 'before-save-hook #'delete-trailing-whitespace)

;; use tabs instead of spaces
(setq-default indent-tabs-mode nil)

;; automatically follow symlinks
(setq vc-follow-symlinks t)

;; fullscreen settings
(setq ns-use-native-fullscreen nil)

;; auto-close parens, brackets, quotes
(electric-pair-mode 1)

(use-package oauth2
  :init
  ;; necessary to remove warnings: https://emacs.stackexchange.com/questions/37036/where-are-these-variables-defined-bytecomp-warnings
  (defvar url-http-extra-headers ())
  (defvar oauth--token-data ())
  (defvar url-callback-function ())
  (defvar url-callback-arguments ()))

(use-package smex
  :general
  ("M-x" 'smex)
  :ghook ('ido-setup-hook #'wpc/bind-ido-keys)
  :config
  (smex-initialize))

(use-package flx-ido
  :after (smex)
  :config
  (flx-ido-mode 1)
  (setq ido-enable-flex-matching t
        ido-use-faces nil))

(use-package swiper
  :general
  ("C-s" 'swiper
   "C-r" 'swiper))

(use-package yasnippet
  :config
  (yas-global-mode 1))

(use-package ace-window
  :general
  ("C-x o" 'ace-window)
  :config
  (setq aw-keys '(?a ?s ?d ?f ?j ?k ?k ?\;)))

(use-package projectile
  :config
  (projectile-mode t))

(use-package counsel
  :config
  (defun wpc/counsel-git-grep ()
    (interactive)
    (let ((maybe-symbol (wpc/string-symbol-at-point)))
      (if (string= maybe-symbol "nil")
          (counsel-git-grep)
        (counsel-git-grep nil maybe-symbol)))))

;; projectile intergration with ivy
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

(use-package markdown-mode)
(use-package yaml-mode)

(provide 'wpc-misc)
;;; misc.el ends here
