;;; email.el --- My Emacs email settings -*- lexical-binding: t -*-
;; Author: William Carroll <wpcarro@gmail.com>

;;; Commentary:
;; Attempting to configure to `notmuch' for my personal use.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dependencies
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'notmuch)
(require 'list)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq notmuch-saved-searches
      '((:name "inbox" :query "tag:inbox" :key "i")
        (:name "direct" :query "tag:direct and tag:unread and not tag:sent" :key "d")
        (:name "action" :query "tag:action" :key "a")
        (:name "review" :query "tag:review" :key "r")
        (:name "waiting" :query "tag:waiting" :key "w")
        (:name "broadcast" :query "tag:/broadcast\/.+/ and tag:unread" :key "b")
        (:name "systems" :query "tag:/systems\/.+/ and tag:unread" :key "s")
        (:name "sent" :query "tag:sent" :key "t")
        (:name "drafts" :query "tag:draft" :key "D")))

;; Sort results from newest-to-oldest.
(setq notmuch-search-oldest-first nil)

;; Discard noisy email signatures.
(setq notmuch-mua-cite-function #'message-cite-original-without-signature)

;; By default, this is just '("-inbox")
(setq notmuch-archive-tags '("-inbox" "-unread" "+archive"))

;; Show saved searches even when they're empty.
(setq notmuch-show-empty-saved-searches t)

;; Currently the sendmail executable on my system is symlinked to msmtp.
(setq send-mail-function #'sendmail-send-it)

;; I'm not sure if I need this or not. Copying it from tazjin@'s monorepo.
(setq notmuch-always-prompt-for-sender nil)

;; Add the "User-Agent" header to my emails and ensure that it includes Emacs
;; and notmuch information.
(setq notmuch-mua-user-agent-function
      (lambda ()
        (format "Emacs %s; notmuch.el %s" emacs-version notmuch-emacs-version)))

;; I was informed that Gmail does this server-side
(setq notmuch-fcc-dirs nil)

;; Ensure buffers are closed after sending mail.
(setq message-kill-buffer-on-exit t)

;; Ensure sender is correctly passed to msmtp.
(setq mail-specify-envelope-from t
      message-sendmail-envelope-from 'header
      mail-envelope-from 'header)

;; Assert that no two saved searches share share a KBD
(prelude/assert
 (list/xs-distinct-by? (lambda (x) (plist-get x :key)) notmuch-saved-searches))

(provide 'email)
;;; email.el ends here
