;;; passively.el --- Passively learn new information -*- lexical-binding: t; -*-
;;
;; SPDX-License-Identifier: MIT
;; Copyright (C) 2020 The TVL Contributors
;;
;; Author: Vincent Ambo <tazjin@tvl.su>
;; Version: 1.0
;; Package-Requires: (ht seq)
;; URL: https://code.tvl.fyi/about/tools/emacs-pkgs/passively/
;;
;; This file is not part of GNU Emacs.

(require 'ht)
(require 'seq)

;; Customisation options

(defgroup passively nil
  "Customisation options for passively"
  :group 'applications)

(defcustom passively-learn-terms nil
  "Terms that passively should randomly display to the user. The
format of this variable is a hash table with a string key that
uniquely identifies the term, and a string value that is
displayed to the user.

For example, a possible value could be:

   (ht (\"забыть\" \"забыть - to forget\")
       (\"действительно\" \"действительно - indeed, really\")))
"
  ;; TODO(tazjin): No hash-table type in customization.el?
  :type '(sexp)
  :group 'passively)

(defcustom passively-store-state (format "%spassively.el" user-emacs-directory)
  "File in which passively should store its state (e.g. known terms)"
  :type '(file)
  :group 'passively)

(defcustom passively-show-after-idle-for 4
  "Number of seconds after Emacs goes idle that passively should
wait before displaying a term."
  :type '(integer)
  :group 'passively)

;; Implementation of state persistence
(defvar passively-last-displayed nil
  "Key of the last displayed passively term.")

(defvar passively--known-terms (make-hash-table)
  "Set of terms that are already known.")

(defun passively--persist-known-terms ()
  "Persist the set of known passively terms to disk."
  (with-temp-file passively-store-state
    (insert (prin1-to-string (ht-keys passively--known-terms)))))

(defun passively--load-known-terms ()
  "Load the set of known passively terms from disk."
  (with-temp-buffer
    (insert-file-contents passively-store-state)
    (let ((keys (read (current-buffer))))
      (setq passively--known-terms (make-hash-table))
      (seq-do
       (lambda (key) (ht-set passively--known-terms key t))
       keys)))
  (message "passively: loaded %d known words"
           (seq-length (ht-keys passively--known-terms))))

(defun passively-mark-last-as-known ()
  "Mark the last term that passively displayed as known. It will
not be displayed again."
  (interactive)

  (ht-set passively--known-terms passively-last-displayed t)
  (passively--persist-known-terms)
  (message "passively: Marked '%s' as known" passively-last-displayed))

;; Implementation of main display logic
(defvar passively--display-timer nil
  "idle-timer used for displaying terms by passively")

(defun passively--random-term (timeout)
  ;; This is stupid, calculate set intersections instead.
  (if (< 1000 timeout)
      (error "It seems you already know all the terms?")
    (seq-random-elt (ht-keys passively-learn-terms))))

(defun passively--display-random-term ()
  (let* ((timeout 1)
         (term (passively--random-term timeout)))
    (while (ht-contains? passively--known-terms term)
      (setq timeout (+ 1 timeout))
      (setq term (passively--random-term timeout)))
    (setq passively-last-displayed term)
    (message (ht-get passively-learn-terms term))))

(defun passively-enable ()
  "Enable automatic display of terms via passively."
  (interactive)
  (if passively--display-timer
      (error "passively: Already running!")
    (passively--load-known-terms)
    (setq passively--display-timer
          (run-with-idle-timer passively-show-after-idle-for t
                               #'passively--display-random-term))
    (message "passively: Now running after %s seconds of idle time"
             passively-show-after-idle-for)))

(defun passively-disable ()
  "Turn off automatic display of terms via passively."
  (interactive)
  (unless passively--display-timer
    (error "passively: Not running!"))
  (cancel-timer passively--display-timer)
  (setq passively--display-timer nil)
  (message "passively: Now disabled"))

(provide 'passively)
