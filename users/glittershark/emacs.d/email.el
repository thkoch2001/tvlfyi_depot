;;; ~/code/depot/users/glittershark/emacs.d/email.el -*- lexical-binding: t; -*-

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
        message-send-mail-function 'message-send-mail-with-sendmail)

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
        message-send-mail-function 'message-send-mail-with-sendmail)
