;;; wpc-rust.el --- Support Rust language -*- lexical-binding: t -*-

;; Author: William Carroll <wpcarro@gmail.com>
;; Version: 0.0.1
;; Package-Requires: ((emacs "24"))

;;; Commentary:
;; Supports my Rust work.
;;
;; Dependencies:
;; - `rustup`
;; - `rustup component add rust-src`
;; - `rustup toolchain add nightly && cargo +nightly install racer`

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dependencies
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'macros)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package racer
  :config
  (setq rust-sysroot (->> "~/.cargo/bin/rustc --print sysroot"
                          shell-command-to-string
                          s-trim-right))
  (setq racer-rust-src-path (f-join rust-sysroot "lib/rustlib/src/rust/src"))
  (add-hook 'racer-mode-hook #'eldoc-mode))

(use-package rust-mode
  :config
  (add-hook 'rust-mode-hook #'racer-mode)
  (macros-add-hook-before-save 'rust-mode-hook #'rust-format-buffer)
  (define-key rust-mode-map
    (kbd "TAB")
    #'company-indent-or-complete-common)
  (define-key rust-mode-map
    (kbd "M-d")
    #'racer-describe))

(provide 'wpc-rust)
;;; wpc-rust.el ends here
