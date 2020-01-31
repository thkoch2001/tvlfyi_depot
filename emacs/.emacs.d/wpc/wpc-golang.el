;;; wpc-ocaml.el --- Tooling preferences for Go -*- lexical-binding: t -*-
;; Author: William Carroll <wpcarro@gmail.com>

;;; Commentary:
;; Tooling support for golang development.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; I'm unsure if this belongs in wpc-golang.el because it's a generic setting,
;; but because go is the first languages I've encountered that enforces tab
;; usage (I think) I'm configuring it.
(setq-default tab-width 4)

(use-package go-mode
  :config
  (add-hook-before-save 'go-mode-hook #'gofmt-before-save))

(provide 'wpc-golang)
;;; wpc-ocaml.el ends here
