;;; ../code/depot/users/glittershark/emacs.d/rust.el -*- lexical-binding: t; -*-

; (setq lsp-rust-analyzer-cargo-watch-command "clippy")

(add-to-list 'auto-mode-alist '("\\.rs$" . rust-mode))

(defun grfn/rust-setup ()
  (interactive)
  (setq lsp-rust-server 'rust-analyzer)
  (setq-local whitespace-line-column 100
              fill-column 100)
  (setq-local rustic-format-trigger 'on-save)
  (setq rust-format-show-buffer nil)
  (rust-enable-format-on-save)
  (lsp))

(add-hook 'rust-mode-hook #'grfn/rust-setup)

(map!
 (:map rust-mode-map
  :n "g RET" #'cargo-process-current-file-tests
  :n "g R" #'lsp-find-references
  (:localleader
   "m" #'lsp-rust-analyzer-expand-macro)))
