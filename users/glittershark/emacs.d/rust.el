;;; ../code/depot/users/glittershark/emacs.d/rust.el -*- lexical-binding: t; -*-

(defun grfn/rust-setup ()
  (setq lsp-rust-server 'rust-analyzer)
  (rust-enable-format-on-save)
  (lsp))

(add-hook 'rust-mode-hook #'grfn/rust-setup)
