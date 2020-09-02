;;; wpc-org.el --- My org preferences -*- lexical-binding: t -*-

;; Author: William Carroll <wpcarro@gmail.com>
;; Version: 0.0.1
;; Package-Requires: ((emacs "24.1"))
;; Homepage: https://user.git.corp.google.com/wpcarro/briefcase

;;; Commentary:
;; Hosts my org mode preferences

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dependencies
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'f)
(require 'macros)
(require 'general)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package org
  :config
  (evil-set-initial-state 'org-mode 'normal)
  (general-add-hook 'org-mode-hook
                    (list (macros-disable linum-mode)
                          (macros-disable company-mode)))
  (setq org-startup-folded nil)
  (setq org-todo-keywords '((sequence "TODO" "BLOCKED" "DONE")))
  (general-unbind 'normal org-mode-map "M-h" "M-j" "M-k" "M-l"))

(use-package org-bullets
  :config
  (general-add-hook 'org-mode-hook (macros-enable org-bullets-mode)))

(provide 'wpc-org)
;;; wpc-org.el ends here
