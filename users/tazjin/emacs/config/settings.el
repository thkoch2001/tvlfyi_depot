(require 'uniquify)

;; We don't live in the 80s, but we're also not a shitty web app.
(setq gc-cons-threshold 20000000)

(setq uniquify-buffer-name-style 'forward)

; Fix some defaults
(setq visible-bell nil
      inhibit-startup-message t
      color-theme-is-global t
      sentence-end-double-space nil
      shift-select-mode nil
      uniquify-buffer-name-style 'forward
      whitespace-style '(face trailing lines-tail tabs)
      whitespace-line-column 80
      default-directory "~"
      fill-column 80
      ediff-split-window-function 'split-window-horizontally
      initial-major-mode 'emacs-lisp-mode)

(add-to-list 'safe-local-variable-values '(lexical-binding . t))
(add-to-list 'safe-local-variable-values '(whitespace-line-column . 80))

(set-default 'indent-tabs-mode nil)

;; UTF-8 please
(setq locale-coding-system 'utf-8) ; pretty
(set-terminal-coding-system 'utf-8) ; pretty
(set-keyboard-coding-system 'utf-8) ; pretty
(set-selection-coding-system 'utf-8) ; please
(prefer-coding-system 'utf-8) ; with sugar on top

;; Make emacs behave sanely (overwrite selected text)
(delete-selection-mode 1)

;; Keep your temporary files in tmp, emacs!
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))

(remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function)

;; Show time in 24h format
(setq display-time-24hr-format t)

;; Use python-mode for Starlark files.
(add-to-list 'auto-mode-alist '("\\.star\\'" . python-mode))

(provide 'settings)
