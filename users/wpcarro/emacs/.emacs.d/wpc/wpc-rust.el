;;; wpc-rust.el --- Support Rust language -*- lexical-binding: t -*-

;; Author: William Carroll <wpcarro@gmail.com>
;; Version: 0.0.1
;; Package-Requires: ((emacs "24"))

;;; Commentary:
;; Supports my Rust work.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package rust-mode
  :config
  (setq lsp-rust-server #'rust-analyzer)
  (setq rust-format-show-buffer nil)
  (add-hook 'rust-mode-hook #'lsp)
  (setq rust-format-on-save t))

(provide 'wpc-rust)
;;; wpc-rust.el ends here
