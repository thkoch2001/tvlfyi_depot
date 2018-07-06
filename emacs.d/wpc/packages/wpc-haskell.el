;;; haskell.el --- My Haskell preferences -*- lexical-binding: t -*-
;; Author: William Carroll <wpcarro@gmail.com>

;;; Commentary:
;; Hosts my Haskell development preferences

;;; Code:

;; Haskell support
(use-package intero
  :config
  (intero-global-mode 1))

;; text objects for Haskell
(quelpa '(evil-text-objects-haskell
          :fetcher github
          :repo "urbint/evil-text-objects-haskell"))
(require 'evil-text-objects-haskell)

(use-package haskell-mode
  :gfhook #'evil-text-objects-haskell/install
  :after (intero evil-text-objects-haskell)
  :config
  (flycheck-add-next-checker 'intero 'haskell-hlint)
  (let ((m-symbols
         '(("`mappend`" . "⊕")
           ("<>"        . "⊕"))))
    (dolist (item m-symbols) (add-to-list 'haskell-font-lock-symbols-alist item)))
  (setq haskell-font-lock-symbols t)
  (add-hook 'before-save-hook #'haskell-align-imports))


(defun empire/haskell/module->test ()
  "Jump from a module to a test."
  (let ((filename (->> buffer-file-name
                       (s-replace "/src/" "/test/")
                       (s-replace ".hs" "Test.hs")
                       find-file)))
    (make-directory (f-dirname filename) t)
    (find-file filename)))

(defun empire/haskell/test->module ()
  "Jump from a test to a module."
  (let ((filename (->> buffer-file-name
                       (s-replace "/test/" "/src/")
                       (s-replace "Test.hs" ".hs")
                       )))
    (make-directory (f-dirname filename) t)
    (find-file filename)))

(defun empire/haskell/test<->module ()
  "Toggle between test and module in Haskell."
  (interactive)
  (if (s-contains? "/src/" buffer-file-name)
      (empire/haskell/module->test)
    (empire/haskell/test->module)))

(provide 'wpc-haskell)
;;; wpc-haskell.el ends here
