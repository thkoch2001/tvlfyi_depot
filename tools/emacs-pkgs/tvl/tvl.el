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
  :group 'tvl)

(defcustom tvl-depot-path "/depot"
  "Location at which the TVL depot is checked out."
  :group 'tvl)

(defun tvl--gerrit-ref (target-branch &optional flags)
  (let ((flag-suffix (if flags (format "%%l=%s" (s-join "," flags))
                       "")))
    (format "HEAD:refs/for/%s%s" target-branch flag-suffix)))

(define-suffix-command magit-gerrit-push-for-review ()
  "Push to Gerrit for review."
  (interactive)
  (magit-push-refspecs tvl-gerrit-remote
                       (tvl--gerrit-ref "master")
                       nil))

(transient-append-suffix
  #'magit-push ["r"]
  (list "R" "push to Gerrit for review" #'magit-gerrit-push-for-review))

(define-suffix-command magit-gerrit-submit ()
  "Push to Gerrit for review."
  (interactive)
  (magit-push-refspecs tvl-gerrit-remote
                       (tvl--gerrit-ref "master" '("submit"))
                       nil))

(transient-append-suffix
  #'magit-push ["r"]
  (list "S" "push to Gerrit to submit" #'magit-gerrit-submit))


(define-suffix-command magit-gerrit-rubberstamp ()
  "Push, automatically approve and submit to Gerrit. This
rubberstamp operation is dangerous and should only be used in
`//users'."
  (interactive)
  (magit-push-refspecs tvl-gerrit-remote
                       (tvl--gerrit-ref "master"
                                        '("Code-Review+2" "publish-comments" "submit"))
                       nil))

(transient-append-suffix
  #'magit-push ["r"]
  (list "P" "push, rubberstamp & submit to Gerrit" #'magit-gerrit-rubberstamp))

(defun tvl-depot-status ()
  "Open the TVL monorepo in magit."
  (interactive)
  (magit-status tvl-depot-path))

(provide 'tvl)
;;; tvl.el ends here
