;;; tvl.el --- description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 Griffin Smith
;; Copyright (C) 2020 The TVL Contributors
;;
;; Author: Griffin Smith <grfn@gws.fyi>
;; Version: 0.0.1
;; Package-Requires: (s magit)
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; This file provides shared utilities for interacting with the TVL monorepo
;;
;;; Code:

(require 'magit)
(require 's)

(defgroup tvl nil
  "Customisation options for TVL functionality.")

(defcustom tvl-gerrit-remote "origin"
  "Name of the git remote for gerrit"
  :type '(string)
  :group 'tvl)

(defcustom tvl-depot-path "/depot"
  "Location at which the TVL depot is checked out."
  :type '(string)
  :group 'tvl)

(defcustom tvl-target-branch "canon"
  "Branch to use to target CLs"
  :group 'tvl
  :type '(string)
  :safe (lambda (_) t))

(defun tvl--gerrit-ref (target-branch &optional flags)
  (let ((flag-suffix (if flags (format "%%l=%s" (s-join "," flags))
                       "")))
    (format "HEAD:refs/for/%s%s" target-branch flag-suffix)))

(transient-define-suffix magit-gerrit-push-for-review ()
  "Push to Gerrit for review."
  (interactive)
  (magit-push-refspecs tvl-gerrit-remote
                       (tvl--gerrit-ref tvl-target-branch)
                       nil))

(transient-append-suffix
  #'magit-push ["r"]
  (list "R" "push to Gerrit for review" #'magit-gerrit-push-for-review))

(transient-define-suffix magit-gerrit-push-wip ()
  "Push to Gerrit as a work-in-progress."
  (interactive)
  (magit-push-refspecs tvl-gerrit-remote
                       (concat (tvl--gerrit-ref tvl-target-branch) "%wip")
                       nil))

(transient-append-suffix
  #'magit-push ["r"]
  (list "W" "push to Gerrit as a work-in-progress" #'magit-gerrit-push-wip))

(transient-define-suffix magit-gerrit-submit ()
  "Push to Gerrit for review."
  (interactive)
  (magit-push-refspecs tvl-gerrit-remote
                       (tvl--gerrit-ref tvl-target-branch '("submit"))
                       nil))

(transient-append-suffix
  #'magit-push ["r"]
  (list "S" "push to Gerrit to submit" #'magit-gerrit-submit))


(transient-define-suffix magit-gerrit-rubberstamp ()
  "Push, automatically approve and submit to Gerrit. This
rubberstamp operation is dangerous and should only be used in
`//users'."
  (interactive)
  (magit-push-refspecs tvl-gerrit-remote
                       (tvl--gerrit-ref tvl-target-branch
                                        '("Code-Review+2" "publish-comments"))
                       nil))

(transient-append-suffix
  #'magit-push ["r"]
  (list "P" "push & rubberstamp to Gerrit" #'magit-gerrit-rubberstamp))

(defun tvl-depot-status ()
  "Open the TVL monorepo in magit."
  (interactive)
  (call-interactively (magit-status tvl-depot-path)))

(provide 'tvl)
;;; tvl.el ends here
