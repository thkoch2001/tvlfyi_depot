;;; ~/code/depot/users/glittershark/emacs.d/cpp.el -*- lexical-binding: t; -*-


(load! "google-c-style")

(after! flycheck
  (add-to-list 'flycheck-disabled-checkers 'c/c++-gcc)
  (add-to-list 'flycheck-disabled-checkers 'c/c++-clangd))

(defun +grfn/cpp-setup ()
  (when (s-starts-with?
         "/home/grfn/code/depot/third_party/nix"
         (buffer-file-name))
    (setq lsp-clients-clangd-executable "/home/grfn/code/depot/users/glittershark/emacs.d/nix-clangd.sh"
          lsp-clients-clangd-args nil)
    (google-set-c-style)
    (lsp)))

(add-hook 'c++-mode-hook #'+grfn/cpp-setup)

(comment
 (setq
  lsp-clients-clangd-executable
  "/home/grfn/code/depot/third_party/nix/clangd.sh"
  lsp-clients-clangd-args nil)
 )
