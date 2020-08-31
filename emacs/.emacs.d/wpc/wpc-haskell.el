;;; wpc-haskell.el --- My Haskell preferences -*- lexical-binding: t -*-

;; Author: William Carroll <wpcarro@gmail.com>
;; Version: 0.0.1
;; URL: https://git.wpcarro.dev/wpcarro/briefcase
;; Package-Requires: ((emacs "24"))

;;; Commentary:
;; Hosts my Haskell development preferences

;;; Code:

;; font-locking, glyph support, etc
(use-package haskell-mode
  :config
  (add-hook-before-save 'haskell-mode #'haskell-align-imports))

;; LSP support
(use-package lsp-haskell
  :after (haskell-mode)
  :config
  (setq lsp-haskell-process-path-hie "hie-wrapper")
  (add-hook 'haskell-mode-hook #'lsp-haskell-enable)
  (add-hook 'haskell-mode-hook #'flycheck-mode))

;; Test toggling
(defun wpc-haskell-module->test ()
  "Jump from a module to a test."
  (let ((filename (->> buffer-file-name
                       (s-replace "/src/" "/test/")
                       (s-replace ".hs" "Test.hs")
                       find-file)))
    (make-directory (f-dirname filename) t)
    (find-file filename)))

(defun wpc-haskell-test->module ()
  "Jump from a test to a module."
  (let ((filename (->> buffer-file-name
                       (s-replace "/test/" "/src/")
                       (s-replace "Test.hs" ".hs"))))
    (make-directory (f-dirname filename) t)
    (find-file filename)))

(defun wpc-haskell-test<->module ()
  "Toggle between test and module in Haskell."
  (interactive)
  (if (s-contains? "/src/" buffer-file-name)
      (wpc-haskell-module->test)
    (wpc-haskell-test->module)))

(provide 'wpc-haskell)
;;; wpc-haskell.el ends here
