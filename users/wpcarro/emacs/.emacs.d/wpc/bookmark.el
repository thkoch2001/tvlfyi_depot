;;; bookmark.el --- Saved files and directories on my filesystem -*- lexical-binding: t -*-

;; Author: William Carroll <wpcarro@gmail.com>
;; Version: 0.0.1
;; Package-Requires: ((emacs "24.3"))

;;; Commentary:
;; A more opinionated version of Emacs's builtin `jump-to-register'.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dependencies
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'project)
(require 'general)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl-defstruct bookmark label path kbd)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; API
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun bookmark-open (b)
  "Open bookmark, B, as either a project directory or a regular directory."
  (with-temp-buffer
    (cd (bookmark-path b))
    (call-interactively #'project-find-file)))

(defun bookmark-install-kbd (b)
  "Define two functions to explore B and assign them to keybindings."
  (eval `(defun ,(intern (format "bookmark-visit-%s" (bookmark-label b))) ()
           (interactive)
           (find-file ,(bookmark-path b))))
  (eval `(defun ,(intern (format "bookmark-browse-%s" (bookmark-label b))) ()
           (interactive)
           (bookmark-open ,b)))
  (general-define-key
   :prefix "<SPC>"
   :states '(motion)
   (format "J%s" (bookmark-kbd b)) `,(intern (format "bookmark-visit-%s" (bookmark-label b)))
   (format "j%s" (bookmark-kbd b)) `,(intern (format "bookmark-browse-%s" (bookmark-label b)))))

(provide 'bookmark)
;;; bookmark.el ends here
