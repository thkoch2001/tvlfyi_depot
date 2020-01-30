;;; haskell.el --- My Haskell preferences -*- lexical-binding: t -*-
;; Author: William Carroll <wpcarro@gmail.com>

;;; Commentary:
;; Hosts my Haskell development preferences

;;; Code:

;; Haskell support

;; font-locking, glyph support, etc
(use-package haskell-mode
  :config
  (let ((m-symbols
         '(("`mappend`" . "⊕")
           ("<>"        . "⊕"))))
    (dolist (item m-symbols) (add-to-list 'haskell-font-lock-symbols-alist item)))
  (setq haskell-font-lock-symbols t)
  (add-hook-before-save 'haskell-mode #'haskell-align-imports))

;; LSP support
(use-package lsp-haskell
  :after (haskell-mode)
  :config
  (setq lsp-haskell-process-path-hie "hie-wrapper")
  (add-hook 'haskell-mode-hook #'lsp-haskell-enable)
  (add-hook 'haskell-mode-hook #'flycheck-mode))

;; Test toggling
(defun haskell/module->test ()
  "Jump from a module to a test."
  (let ((filename (->> buffer-file-name
                       (s-replace "/src/" "/test/")
                       (s-replace ".hs" "Test.hs")
                       find-file)))
    (make-directory (f-dirname filename) t)
    (find-file filename)))

(defun haskell/test->module ()
  "Jump from a test to a module."
  (let ((filename (->> buffer-file-name
                       (s-replace "/test/" "/src/")
                       (s-replace "Test.hs" ".hs")
                       )))
    (make-directory (f-dirname filename) t)
    (find-file filename)))

(defun haskell/test<->module ()
  "Toggle between test and module in Haskell."
  (interactive)
  (if (s-contains? "/src/" buffer-file-name)
      (haskell/module->test)
    (haskell/test->module)))

(provide 'wpc-haskell)
;;; wpc-haskell.el ends here
