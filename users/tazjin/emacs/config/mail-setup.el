(require 'notmuch)

;; (global-set-key (kbd "C-c m") 'notmuch-hello)
;; (global-set-key (kbd "C-c C-e n") 'notmuch-mua-new-mail)

(setq notmuch-cache-dir (format "%s/.cache/notmuch" (getenv "HOME")))
(make-directory notmuch-cache-dir t)

;; Cache addresses for completion:
(setq notmuch-address-save-filename (concat notmuch-cache-dir "/addresses"))

;; Don't spam my home folder with drafts:
(setq notmuch-draft-folder "drafts") ;; relative to notmuch database

;; Mark things as read when archiving them:
(setq notmuch-archive-tags '("-inbox" "-unread" "+archive"))

;; Show me saved searches that I care about:
(setq notmuch-saved-searches
      '((:name "inbox" :query "tag:inbox" :count-query "tag:inbox AND tag:unread" :key "i")
        (:name "sent" :query "tag:sent" :key "t")
        (:name "drafts" :query "tag:draft")))
(setq notmuch-show-empty-saved-searches t)

;; Mail sending configuration
(setq sendmail-program "gmi") ;; lieer binary supports sendmail emulation
(setq message-sendmail-extra-arguments
      '("send" "--quiet" "-t" "-C" "~/mail/account.tazjin"))
(setq send-mail-function 'sendmail-send-it)
(setq notmuch-mua-user-agent-function
      (lambda () (format "Emacs %s; notmuch.el %s" emacs-version notmuch-emacs-version)))
(setq mail-host-address (system-name))
(setq notmuch-mua-cite-function #'message-cite-original-without-signature)
(setq notmuch-fcc-dirs nil) ;; Gmail does this server-side
(setq message-signature nil) ;; Insert message signature manually with C-c C-w

;; Close mail buffers after sending mail
(setq message-kill-buffer-on-exit t)

;; Ensure sender is correctly passed to msmtp
(setq mail-specify-envelope-from t
      message-sendmail-envelope-from 'header
      mail-envelope-from 'header)

;; Store sent mail in the correct folder per account
(setq notmuch-maildir-use-notmuch-insert nil)

;; I don't use drafts but I instinctively hit C-x C-s constantly, lets
;; handle that gracefully.
(define-key notmuch-message-mode-map (kbd "C-x C-s") #'ignore)

;; Define a mode-line segment for displaying the count of unread,
;; important mails in the last window's mode-line:
(defvar *last-notmuch-count-redraw* 0)
(defvar *current-notmuch-count* nil)

(defun update-display-notmuch-counts ()
  "Update and render the current state of the notmuch unread
  count for display in the mode-line.

  The offlineimap-timer runs every 2 minutes, so it does not make
  sense to refresh this much more often than that."

  (when (> (- (float-time) *last-notmuch-count-redraw*) 30)
    (setq *last-notmuch-count-redraw* (float-time))
    (let* ((inbox-unread (notmuch-saved-search-count "tag:inbox and tag:unread"))
           (notmuch-count (format "I: %s; D: %s" inbox-unread)))
      (setq *current-notmuch-count* notmuch-count)))

  (when (and (bottom-right-window-p)
             ;; Only render if the initial update is done and there
             ;; are unread mails:
             *current-notmuch-count*
             (not (equal *current-notmuch-count* "I: 0; D: 0")))
    *current-notmuch-count*))

;; TODO(tazjin): re-add this segment to the modeline

(provide 'mail-setup)
