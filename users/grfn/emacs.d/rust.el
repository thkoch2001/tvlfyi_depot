;;; -*- lexical-binding: t; -*-

(add-to-list 'auto-mode-alist '("\\.rs$" . rust-mode))

(defun grfn/rust-setup ()
  (interactive)

  (direnv--maybe-update-environment)

  (+evil-embrace-angle-bracket-modes-hook-h)

  (setq lsp-rust-server 'rust-analyzer)
  (setq-local whitespace-line-column 100
              fill-column 100)
  (setq rust-format-show-buffer nil)
  (setq lsp-rust-analyzer-import-merge-behaviour "last"
        lsp-rust-analyzer-cargo-watch-command "clippy"
        lsp-rust-analyzer-cargo-watch-args ["--target-dir" "/home/grfn/code/readyset/readyset/target/rust-analyzer"]
        lsp-ui-doc-enable t)
  (rust-enable-format-on-save)
  (lsp))

(add-hook 'rust-mode-hook #'grfn/rust-setup)

(map!
 (:map rust-mode-map
  :n "g RET" #'lsp-rust-analyzer-run
  :n "g R" #'lsp-find-references
  :n "g d" #'lsp-find-definition
  :n "g Y" #'lsp-goto-type-definition
  (:localleader
   "m" #'lsp-rust-analyzer-expand-macro)))

(comment
 (flycheck-get-next-checkers 'lsp)
 (flycheck-add-next-checker)
 (flycheck-get-next-checkers 'lsp)
 )

(set-company-backend! 'rust-mode
  '(:separate company-capf company-yasnippet))
