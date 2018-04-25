;;; org.el --- My org preferences -*- lexical-binding: t -*-
;; Author: William Carroll <wpcarro@gmail.com>

;;; Commentary:
;; Hosts my org mode preferences

;;; Code:

;; Griffin's org clubhouse integration
;;(load-file "~/.emacs.d/vendor/org-clubhouse.el")
;;(setq org-clubhouse-auth-token (wpc/read-file-as-string "~/dotfiles/configs/secrets/clubhouse_token.txt")
;;      org-clubhouse-team-name "urbint")
;;(add-hook 'org-mode-hook #'org-clubhouse-mode)

(use-package org
  :ghook (nil (disable linum-mode))
  :general
  (:prefix "C-c"
           "l" 'org-store-link
           "a" 'org-agenda
           "c" 'org-capture)
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
  (setq org-default-notes-file (wpc/org-file "notes"))
  (setq org-log-done 'time)
  (setq org-agenda-files (list (wpc/org-file "work")
                               (wpc/org-file "personal")))
  (setq org-capture-templates
        `(("t" "Todo" entry (file+heading ,(ub/org-file "index") "Ideas")
           "* TODO %?\n  %i"))))

(use-package org-bullets
  :after (org)
  :ghook ('org-mode-hook (enable org-bullets-mode)))

(provide 'wpc-org)
;;; org.el ends here
