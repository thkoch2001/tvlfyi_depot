;;; chrome.el --- Helpers for Google Chrome -*- lexical-binding: t -*-
;; Author: William Carroll <wpcarro@gmail.com>

;;; Commentary:
;; Some helper functions for working with Google Chrome.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dependencies
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'macros)
(require 'alist)
(require 'list)
(require 'general)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Library
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar chrome/install-kbds? t
  "If t, install keybinding.")

;; TODO: Consider modelling this as a rose-tree that can nest itself
;; arbitrarily.
;; TODO: Consider exporting existing chrome bookmarks.
(defconst chrome/label->url
  '(("Google" . "www.google.com")
    ("Hacker News" . "news.ycombinator.com")
    ("Gmail" . "www.gmail.com")
    ("WhatsApp" . "web.whatsapp.com")
    ("Google Chat" . "chat/")
    ("Google Calendar" . "calendar/")
    ("Teknql" . "teknql.slack.com/messages")
    ("Twitter" . "twitter.com"))
  "Mapping labels to urls for my bookmarks.")

(defconst chrome/splash-pages
  '("Google Calendar"
    "Gmail"
    "Google Chat"
    "WhatsApp"
    "Teknql")
  "The pages that should open when I open Chrome.")

;; TODO: Add defensive check to start chrome if it isn't already open.

;; TODO: Support option to create new session even if one already exists.

(defun chrome/open-splash-pages ()
  "Opens Chrome with my preferred splash pages."
  (interactive)
  (->> chrome/splash-pages
       (-map (lambda (x) (alist/get x chrome/label->url)))
       chrome/open-urls))

;; TODO: Support optional kwargs.
(cl-defun chrome/open-url (url &key new-window?)
  "Opens `URL' in google-chrome.
Will open without toolbars if APP-MODE? is t."
  (shell-command (s-concat
                  "google-chrome "
                  (if new-window? "--new-window " "")
                  url)))

(defun chrome/open-urls (urls)
  "Open multiple `URLS' in chrome."
  (chrome/open-url
   (list/join " " urls)))

(defun chrome/browse ()
  "Display a counsel window for browsing URLs."
  (interactive)
  (ivy-read
   "URL: "
   chrome/label->url
   :action (lambda (entry)
             (chrome/open-url (cdr entry)))))

(provide 'chrome)
;;; chrome.el ends here
