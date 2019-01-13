;;; org.el --- My org preferences -*- lexical-binding: t -*-
;; Author: William Carroll <wpcarro@gmail.com>

;;; Commentary:
;; Hosts my org mode preferences

;;; Code:

(use-package org
  :preface
  (defconst wpc-org-directory
    "~/Dropbox/org")
  (defconst ub-org-directory
    "~/Dropbox/sprint-planning-staging")
  (defun wpc/org-file (file)
    (f-join wpc-org-directory (f-swap-ext file "org")))
  (defun ub/org-file (file)
    (f-join ub-org-directory (f-swap-ext file "org")))
  :config
  ; (general-add-hook org-mode-hook (disable linum-mode))
  (general-define-key :prefix "C-c"
           "l" #'org-store-link
           "a" #'org-agenda
           "c" #'org-capture)
  (setq org-default-notes-file (wpc/org-file "notes"))
  (setq org-log-done 'time)
  (setq org-agenda-files (list (wpc/org-file "work")
                               (wpc/org-file "personal")))
  (setq org-capture-templates
        `(("t" "Todo" entry (file+heading ,(ub/org-file "index") "Ideas")
           "* TODO %?\n  %i"))))

(use-package org-bullets
  :after (org)
  :config
  (general-add-hook 'org-mode-hook (enable org-bullets-mode)))

(provide 'wpc-org)
;;; wpc-org.el ends here
