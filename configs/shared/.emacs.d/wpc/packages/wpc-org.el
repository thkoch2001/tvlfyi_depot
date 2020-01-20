;;; org.el --- My org preferences -*- lexical-binding: t -*-
;; Author: William Carroll <wpcarro@gmail.com>

;;; Commentary:
;; Hosts my org mode preferences

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dependencies
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'f)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq org-directory "~/Dropbox/org")

;; TODO: figure out how to nest this in (use-package org ...)
(setq org-capture-templates
      `(("w" "work" entry (file+headline
                           ,(f-join org-directory "work.org")
                           "Tasks")
         "* TODO %?")
        ("p" "personal" entry (file+headline
                               ,(f-join org-directory "personal.org")
                               "Tasks")
         "* TODO %? ")
        ("i" "ideas" entry (file+headline
                            ,(f-join org-directory "ideas.org")
                            "Tasks")
         "* %? ")
        ("s" "shopping list" entry (file+headline
                            ,(f-join org-directory "shopping.org")
                            "Items")
         "* TODO %? ")))

(evil-set-initial-state 'org-mode 'normal)

(use-package org
  :config
  (general-add-hook 'org-mode-hook
                    ;; TODO: consider supporting `(disable (list linum-mode company-mode))'
                    (list (disable linum-mode)
                          (disable company-mode)))
  (general-define-key :prefix "C-c"
           "l" #'org-store-link
           "a" #'org-agenda
           "c" #'org-capture)
  (setq org-startup-folded nil)
  (setq org-todo-keywords
        '((sequence "TODO" "BLOCKED" "DONE")))
  (setq org-default-notes-file (f-join org-directory "notes.org"))
  (setq org-agenda-files (list (f-join org-directory "work.org")
                               (f-join org-directory "personal.org")))
  ;; TODO: troubleshoot why `wpc/kbds-minor-mode', `wpc/ensure-kbds' aren't
  ;; enough to override the following KBDs. See this discussion for more context
  ;; on where the idea came from:
  ;; https://stackoverflow.com/questions/683425/globally-override-key-binding-in-emacs
  (general-unbind 'normal org-mode-map "M-h" "M-j" "M-k" "M-l"))

(use-package org-bullets
  :after (org)
  :config
  (general-add-hook 'org-mode-hook (enable org-bullets-mode)))

(provide 'wpc-org)
;;; wpc-org.el ends here
