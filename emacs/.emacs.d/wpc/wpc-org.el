;;; org.el --- My org preferences -*- lexical-binding: t -*-
;; Author: William Carroll <wpcarro@gmail.com>

;;; Commentary:
;; Hosts my org mode preferences

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dependencies
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'f)
(require 'org-helpers)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package org
  :config
  (evil-set-initial-state 'org-mode 'normal)
  (general-add-hook 'org-mode-hook
                    ;; TODO: consider supporting `(disable (list linum-mode company-mode))'
                    (list (disable linum-mode)
                          (disable company-mode)))
  (setq org-startup-folded nil)
  (setq org-todo-keywords '((sequence "TODO" "BLOCKED" "DONE")))
  ;; TODO: troubleshoot why `wpc/kbds-minor-mode', `wpc/ensure-kbds' aren't
  ;; enough to override the following KBDs. See this discussion for more context
  ;; on where the idea came from:
  ;; https://stackoverflow.com/questions/683425/globally-override-key-binding-in-emacs
  (general-unbind 'normal org-mode-map "M-h" "M-j" "M-k" "M-l"))

(use-package org-bullets
  :config
  (general-add-hook 'org-mode-hook (enable org-bullets-mode)))

(provide 'wpc-org)
;;; wpc-org.el ends here
