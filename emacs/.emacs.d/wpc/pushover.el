;;; pushover.el --- Send generic messages to mobile device -*- lexical-binding: t -*-
;; Author: William Carroll <wpcarro@gmail.com>

;;; Commentary:
;; Pushover.net is a mobile app that accepts JSON.  This supports loose
;; integration between things and mobile devices.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dependencies
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'request)
(require 'password-store)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Library
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst pushover/app-token
  (password-store-get-field "api-keys/pushover.net" "emacs")
  "App token for \"emacs\" application.")

(defconst pushover/user-key
  (password-store-get "api-keys/pushover.net")
  "Key that identifies me to pushover.")

(defconst pushover/url
  "https://api.pushover.net/1/messages.json"
  "URL to POST messages.")

;; TODO: Rename module "pushover".

(defun pushover/notify (message)
  "Posts MESSAGE to all devices.
Here are the parameters that Pushover accepts:

Required parameters:
  - token - your application's API token
  - user - the user/group key (not e-mail address) of your user (or you),
    viewable when logged into our dashboard (often referred to as USER_KEY in
    our documentation and code examples)
  - message - your message

Additional parameters (optional):
  - attachment - an image attachment to send with the message; see attachments
    for more information on how to upload files
    device - your user's device name to send the message directly to that
    device, rather than all of the user's devices (multiple devices may be
    separated by a comma)
  - title - your message's title, otherwise your app's name is used
  - url - a supplementary URL to show with your message
  - url_title - a title for your supplementary URL, otherwise just the URL is
    shown
  - priority - send as -2 to generate no notification/alert, -1 to always send
    as a quiet notification, 1 to display as high-priority and bypass the user's
    quiet hours, or 2 to also require confirmation from the user
  - sound - the name of one of the sounds supported by device clients to
    override the user's default sound choice
  - timestamp - a Unix timestamp"
  (request
   pushover/url
   :type "POST"
   :params `(("token"   . ,pushover/app-token)
             ("user"    . ,pushover/user-key)
             ("message" . ,message))
   :data nil
   :parser 'json-read
   :success (cl-function
             (lambda (&key data &allow-other-keys)
               (message "Pushover.net notification sent!")))))

(provide 'pushover)
;;; pushover.el ends here
