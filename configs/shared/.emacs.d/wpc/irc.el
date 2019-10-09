;;; irc.el --- Configuration for IRC chat -*- lexical-binding: t -*-
;; Author: William Carroll <wpcarro@gmail.com>

;;; Commentary:
;; Need to decide which client I will use for IRC.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dependencies
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'erc)
(require 'cycle)
(require 'string)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq erc-rename-buffers t)

(defvar irc/channels-cycle
  (cycle/from-list
   '("#omg" "#london" "#panic" "#prod-team"))
  "List of channels through which I can cycle.")

;; Setting `erc-join-buffer' to 'bury prevents erc from stealing focus of the
;; current buffer when it connects to IRC servers.
(setq erc-join-buffer 'bury)

(setq erc-autojoin-channels-alist
      `(("corp.google.com" . ,(cycle/to-list irc/channels-cycle))))

(defcustom irc/install-kbds? t
  "When t, install the keybindings defined herein.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Library
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun irc/message (x)
  "Print message X in a structured way."
  (message (string/format "[irc.el] %s" x)))

(defun irc/connect-to-google ()
  "Connect to Google's Corp IRC using ERC."
  (interactive)
  (erc-ssl :server "irc.corp.google.com"
           :port 6697
           :nick "wpcarro"
           :full-name "William Carroll"))

(defun irc/next-channel ()
  "Join the next channel in `irc/channels-cycle'."
  (interactive)
  (erc-join-channel
   (cycle/next irc/channels-cycle))
  (irc/message
   (string/format "Current IRC channel: %s"
                  (cycle/current irc/channels-cycle))))

(defun irc/prev-channel ()
  "Join the previous channel in `irc/channels-cycle'."
  (interactive)
  (erc-join-channel
   (cycle/prev irc/channels-cycle))
  (irc/message
   (string/format "Current IRC channel: %s"
                  (cycle/current irc/channels-cycle))))

(when irc/install-kbds?
  (general-define-key
   :keymaps 'erc-mode-map
   "<C-tab>" #'irc/next-channel
   "<C-S-iso-lefttab>" #'irc/prev-channel))

(provide 'irc)
;;; irc.el ends here
