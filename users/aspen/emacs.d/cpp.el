;;; -*- lexical-binding: t; -*-


(load! "google-c-style")

(after! flycheck
  (add-to-list 'flycheck-disabled-checkers 'c/c++-gcc)
  (add-to-list 'flycheck-disabled-checkers 'c/c++-clang))

(defun +aspen/cpp-setup ()
  (when (s-starts-with?
         "/home/aspen/code/depot/third_party/nix"
         (buffer-file-name))
    (setq lsp-clients-clangd-executable "/home/aspen/code/depot/users/aspen/emacs.d/nix-clangd.sh"
          lsp-clients-clangd-args nil)
    (google-set-c-style)
    (lsp)
    (add-to-list 'flycheck-disabled-checkers 'c/c++-gcc)
    (add-to-list 'flycheck-disabled-checkers 'c/c++-clang)))

(add-hook 'c++-mode-hook #'+aspen/cpp-setup)

(use-package! protobuf-mode)

(use-package! clang-format+
  :config
  (add-hook 'c-mode-common-hook #'clang-format+-mode))

(map!
 (:map c++-mode-map
  :leader
  (:n "/ i" #'counsel-semantic-or-imenu)))

(comment
 (setq
  lsp-clients-clangd-executable
  "/home/aspen/code/depot/third_party/nix/clangd.sh"
  lsp-clients-clangd-args nil)
 )
