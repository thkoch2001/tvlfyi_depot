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
(require 'prelude)
(require 'alist)
(require 'set)
(require 'maybe)
(require 'macros)
(require 'password-store)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst irc/enable-tests? t
  "When t, run the tests defined herein.")

(setq erc-rename-buffers t)

;; TODO: Find a way to avoid putting "freenode" and "#freenode" as channels
;; here.  I'm doing it because when erc first connects, it's `(buffer-name)' is
;; "freenode", so when `irc/next-channel' is called, it 404s on the
;; `cycle/contains?' call in `irc/channel->cycle" unless "freenode" is there. To
;; make matters even uglier, when `erc-join-channel' is called with "freenode"
;; as the value, it connects to the "#freenode" channel, so unless "#freenode"
;; exists in this cycle also, `irc/next-channel' breaks again.  This doesn't
;; pass my smell test.
(defconst irc/server->channels
  `(("irc.freenode.net"    . ,(cycle/new "freenode" "#freenode" "#nixos" "#emacs" "#pass"))
    ("irc.corp.google.com" . ,(cycle/new "#omg" "#london" "#panic" "#prod-team")))
  "Mapping of IRC servers to a cycle of my preferred channels.")

;; TODO: Assert that no two servers have a channel with the same name. We need
;; this because that's the assumption that underpins the `irc/channel->server'
;; function. This will probably be an O(n^2) operation.
(prelude/assert
 (set/distinct? (set/from-list
                 (cycle/to-list
                  (alist/get "irc.freenode.net"
                             irc/server->channels)))
                (set/from-list
                 (cycle/to-list
                  (alist/get "irc.corp.google.com"
                             irc/server->channels)))))

(defun irc/channel->server (server->channels channel)
  "Resolve an IRC server from a given CHANNEL."
  (let ((result (alist/find (lambda (k v) (cycle/contains? channel v))
                            server->channels)))
    (prelude/assert (maybe/some? result))
    result))

(defun irc/channel->cycle (server->channels channel)
  "Resolve an IRC's channels cycle from a given CHANNEL."
  (alist/get (irc/channel->server server->channels channel)
             server->channels))

;; Setting `erc-join-buffer' to 'bury prevents erc from stealing focus of the
;; current buffer when it connects to IRC servers.
(setq erc-join-buffer 'bury)

;; TODO: Here is another horrible hack that should be revisted.
(setq erc-autojoin-channels-alist
      (->> irc/server->channels
           (alist/map-values #'cycle/to-list)
           (alist/map-keys (>> (s-chop-prefix "irc.")
                               (s-chop-suffix ".net")))))

(defcustom irc/install-kbds? t
  "When t, install the keybindings defined herein.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Library
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun irc/message (x)
  "Print message X in a structured way."
  (message (string/format "[irc.el] %s" x)))

;; TODO: Integrate Google setup with Freenode setup.

;; TODO: Support function or KBD for switching to an ERC buffer.

(defun irc/kill-all-erc-processes ()
  "Kills all ERC buffers and processes."
  (interactive)
  (->> (erc-buffer-list)
       (-map #'kill-buffer)))

(defun irc/switch-to-erc-buffer ()
  "Switch to an ERC buffer."
  (interactive)
  (let ((buffers (erc-buffer-list)))
    (if (list/empty? buffers)
        (error "[irc.el] No ERC buffers available")
      (switch-to-buffer (list/head (erc-buffer-list))))))

(defun irc/connect-to-freenode ()
  "Connect to Freenode IRC."
  (interactive)
  (erc-ssl :server "irc.freenode.net"
           :port 6697
           :nick "wpcarro"
           :password (password-store-get "programming/irc/freenode")
           :full-name "William Carroll"))

;; TODO: Handle failed connections.
(defun irc/connect-to-google ()
  "Connect to Google's Corp IRC using ERC."
  (interactive)
  (erc-ssl :server "irc.corp.google.com"
           :port 6697
           :nick "wpcarro"
           :full-name "William Carroll"))

;; TODO: Prefer defining these with a less homespun solution. There is a
;; function call `erc-buffer-filter' that would be more appropriate for the
;; implementation of `irc/next-channel' and `irc/prev-channel'.
(defun irc/next-channel ()
  "Join the next channel for the active server."
  (interactive)
  (with-current-buffer (current-buffer)
    (let ((cycle (irc/channel->cycle irc/server->channels (buffer-name))))
      (erc-join-channel
       (cycle/next cycle))
      (irc/message
       (string/format "Current IRC channel: %s" (cycle/current cycle))))))

(defun irc/prev-channel ()
  "Join the previous channel for the active server."
  (interactive)
  (with-current-buffer (current-buffer)
    (let ((cycle (irc/channel->cycle irc/server->channels (buffer-name))))
      (erc-join-channel
       (cycle/prev cycle))
      (irc/message
       (string/format "Current IRC channel: %s" (cycle/current cycle))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keybindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when irc/install-kbds?
  (general-define-key
   :keymaps 'erc-mode-map
   "<C-tab>" #'irc/next-channel
   "<C-S-iso-lefttab>" #'irc/prev-channel))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when irc/enable-tests?
  (prelude/assert
   (equal
    (irc/channel->server `(("irc.dairy.com" . ,(cycle/new "#cheese" "#milk"))
                           ("irc.color.com" . ,(cycle/new "#red" "#blue")))
                         "#cheese")
    "irc.dairy.com")))

(provide 'irc)
;;; irc.el ends here
