;;; ~/code/depot/users/glittershark/emacs.d/slack.el -*- lexical-binding: t; -*-

(after! slack
  (set-face-foreground 'slack-message-output-header +solarized-s-base01)
  (set-face-attribute 'slack-message-output-header nil :underline nil)
  (set-face-attribute 'slack-message-output-text nil :height 1.0))

(require 'slack)
(setq slack-buffer-emojify 't
      slack-prefer-current-team 't
      slack-thread-also-send-to-room nil)

(set-popup-rule! "^\\*Slack"
  :quit nil
  :select t
  :side 'bottom
  :ttl nil
  :size 0.3)

(add-hook #'slack-message-buffer-mode-hook
          (lambda () (toggle-truncate-lines -1)))

(map! (:map slack-message-buffer-mode-map
       :n "q" #'delete-window))
