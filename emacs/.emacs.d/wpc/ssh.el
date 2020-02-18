;;; ssh.el --- When working remotely -*- lexical-binding: t -*-
;; Author: William Carroll <wpcarro@gmail.com>

;;; Commentary:
;; Configuration to make remote work easier.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dependencies
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'tramp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: Is "ssh" preferable to "scp"?
(setq tramp-default-method "ssh")

;; Taken from: https://superuser.com/questions/179313/tramp-waiting-for-prompts-from-remote-shell
(setq tramp-shell-prompt-pattern "^[^$>\n]*[#$%>] *\\(\[[0-9;]*[a-zA-Z] *\\)*")

;; Sets the value of the TERM variable to "dumb" when logging into the remote
;; host. This allows me to check for the value of "dumb" in my shell's init file
;; and control the startup accordingly. You can see in the (shamefully large)
;; commit, 0b4ef0e, that I added a check like this to my ~/.zshrc. I've since
;; switched from z-shell to fish. I don't currently have this check in
;; config.fish, but I may need to add it one day soon.
(setq tramp-terminal-type "dumb")

;; Maximizes the tramp debugging noisiness while I'm still learning about tramp.
(setq tramp-verbose 10)

(defun ssh/desktop-cd-home ()
  "Open a dired buffer of my desktop's home directory for wpcarro."
  (interactive)
  (find-file "/ssh:wpcarro@desktop:~"))

(provide 'ssh)
;;; ssh.el ends here
