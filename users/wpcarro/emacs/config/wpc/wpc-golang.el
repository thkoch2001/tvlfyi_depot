;;; wpc-golang.el --- Tooling preferences for Go -*- lexical-binding: t -*-

;; Author: William Carroll <wpcarro@gmail.com>
;; Version: 0.0.1
;; Package-Requires: ((emacs "24"))

;;; Commentary:
;; Tooling support for golang development.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dependencies
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'prelude)
(require 'macros)
(require 'general)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: Support jumping to go source code for fmt.Println, etc.

(use-package go-mode
  :config
  (setq gofmt-command "goimports")
  ;; TODO: Consider configuring `xref-find-definitions' to use `godef-jump'
  ;; instead of shadowing the KBD here.
  (general-define-key
   :states '(normal)
   :keymaps '(go-mode-map)
   "M-." #'godef-jump)
  ;; Support calling M-x `compile'.
  (add-hook 'go-mode-hook (lambda ()
                            (setq-local tab-width 2)
                            (setq-local compile-command "go build -v")))
  (macros-add-hook-before-save 'go-mode-hook #'gofmt-before-save))

(provide 'wpc-golang)
;;; wpc-golang.el ends here
