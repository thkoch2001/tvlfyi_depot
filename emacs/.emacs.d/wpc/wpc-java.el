;;; wpc-java.el --- Java configuration -*- lexical-binding: t -*-
;; Author: William Carroll <wpcarro@gmail.com>

;;; Commentary:
;; When life gets you down, and you find yourself writing Java, remember: at
;; least you're using Emacs.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dependencies
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'prelude)
(require 'macros)

(prelude/assert
 (prelude/executable-exists? "google-java-format"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: Troubleshoot why this isn't running.
(add-hook-before-save
 'java-mode-hook
 (lambda ()
   (call-interactively
    #'google-java-format)))

(add-hook 'java-mode-hook
          (lambda ()
            (setq c-basic-offset 2
                  tab-width 2)))

;; TODO: Figure out whether I should use this or google-emacs.
;; (use-package lsp-java
;;   :config
;;   (add-hook 'java-mode-hook #'lsp))

(provide 'wpc-java)
;;; wpc-java.el ends here
