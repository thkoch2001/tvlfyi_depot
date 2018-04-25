;;; slack.el --- Slack settings -*- lexical-binding: t -*-
;; Author: William Carroll <wpcarro@gmail.com>

;;; Commentary:
;; Wrangling the Slack client in Emacs

;;; Code:

;; Griffin's Slack plugin
;;(defconst slack/token (wpc/read-file-as-string "~/dotfiles/configs/secrets/slack_token.txt"))
;;(defconst wpc/slack-client-secret (wpc/read-file-as-string "~/dotfiles/configs/secrets/slack-client-secret"))
(defconst wpc/slack-client-secret "uncomment above line one day")
(load-file "~/.emacs.d/vendor/slack-snippets.el")

;; Slack client
(use-package slack
  :general
  (n slack-info-mode-map
     :prefix ","
     "u" 'slack-room-update-messages)
  (n slack-mode-map
     :prefix ","
     "c"  'slack-buffer-kill
     "ra" 'slack-message-add-reaction
     "rr" 'slack-message-remove-reaction
     "rs" 'slack-message-show-reaction-users
     "pl" 'slack-room-pins-list
     "pa" 'slack-message-pins-add
     "pr" 'slack-message-pins-remove
     "mm" 'slack-message-write-another-buffer
     "me" 'slack-message-edit
     "md" 'slack-message-delete
     "u"  'slack-room-update-messages
     "2"  'slack-message-embed-mention
     "3"  'slack-message-embed-channel)
  (n slack-mode-map
     "C-n" 'slack-buffer-goto-next-message
     "C-p" 'slack-buffer-goto-prev-message)
  (n slack-edit-message-mode-map
     :prefix ","
     "k" 'slack-message-cancel-edit
     "s" 'slack-message-send-from-buffer
     "2" 'slack-message-embed-mention
     "3" 'slack-message-embed-channel)
  :commands (slack-start)
  :init
  (setq slack-buffer-emojify t) ;; if you want to enable emoji, default nil
  (setq slack-prefer-current-team t)
  :config
  (add-hook 'slack-mode-hook (disable company-mode))
  (setq slack-buffer-function #'switch-to-buffer)
  (slack-register-team
   :name "urbint"
   :default t
   :client-id "william@urbint.com"
   :client-secret wpc/slack-client-secret
   :token slack-token
   :subscribed-channels '(dev dev_questions general random recruiting)
   :full-and-display-names t))

(use-package circe)
(use-package emojify)

(provide 'wpc-slack)
;;; wpc-slack.el ends here
