;;; -*- lexical-binding: t; -*-

(after! notmuch
  (setq notmuch-saved-searches
        '((:name "inbox" :query "tag:inbox tag:important not tag:trash" :key "i")
          (:name "flagged" :query "tag:flagged" :key "f")
          (:name "sent" :query "tag:sent" :key "s")
          (:name "drafts" :query "tag:draft" :key "d")

          (:name "work" :query "tag:inbox and tag:important and path:work/**"
                 :key "w")
          (:name "personal" :query "tag:inbox and tag:important and path:personal/**"
                 :key "p"))
        message-send-mail-function 'message-send-mail-with-sendmail
        message-sendmail-f-is-evil 't
        message-sendmail-envelope-from 'header
        message-sendmail-extra-arguments '("--read-envelope-from"))

  (add-hook! notmuch-message-mode-hook #'notmuch-company-setup))

(setq notmuch-saved-searches
        '((:name "inbox" :query "tag:inbox tag:important not tag:trash" :key "i")
          (:name "flagged" :query "tag:flagged" :key "f")
          (:name "sent" :query "tag:sent" :key "s")
          (:name "drafts" :query "tag:draft" :key "d")

          (:name "work" :query "tag:inbox and tag:important and path:work/**"
                 :key "w")
          (:name "personal" :query "tag:inbox and tag:important and path:personal/**"
                 :key "p"))
        message-send-mail-function 'message-send-mail-with-sendmail
        message-sendmail-f-is-evil 't
        message-sendmail-envelope-from 'header
        message-sendmail-extra-arguments '("--read-envelope-from"))

(set-popup-rule! "^\\*notmuch-saved-search-"
  :ignore t)

(set-popup-rule! (lambda (_ action)
                   (eq (car action)
                       'display-buffer-same-window))
  :ignore t)

(defun apply-thread-patchset (repo branch)
  (interactive "Dgit repo: \nsnew branch name: ")
  (let ((tid notmuch-show-thread-id)
        (tmp "/tmp/notmuch-patchset"))
    (shell-command (format "notmuch-extract-patch %s > %s && ( cd %s && git checkout -b %s && git am %s )"
                           (shell-quote-argument tid)
                           (shell-quote-argument tmp)
                           (shell-quote-argument (expand-file-name repo))
                           (shell-quote-argument branch)
                           (shell-quote-argument tmp)))))
