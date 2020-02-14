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

(setq notmuch-saved-searches '((:name "inbox" :query "tag:inbox" :key "i")
                               (:name "unread" :query "tag:unread" :key "u")
                               (:name "flagged" :query "tag:flagged" :key "f")
                               (:name "sent" :query "tag:sent" :key "t")
                               (:name "drafts" :query "tag:draft" :key "d")
                               (:name "all mail" :query "*" :key "A")
                               (:name "action" :query "tag:action" :key "a")
                               (:name "review" :query "tag:review" :key "r")
                               (:name "waiting" :query "tag:waiting" :key "w")))

;; Sort results from newest-to-oldest.
(setq notmuch-search-oldest-first nil)

;; Assert that no two saved searches share share a KBD
(prelude/assert
 (list/xs-distinct-by? (lambda (x) (plist-get x :key)) notmuch-saved-searches))

(provide 'email)
;;; email.el ends here
