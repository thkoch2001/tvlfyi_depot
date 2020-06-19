;;; tvl.el --- description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 Griffin Smith
;;
;; Author: Griffin Smith <grfn@gws.fyi>
;; Version: 0.0.1
;; Package-Requires: (cl-lib magit)
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; This file provides shared utilities for interacting with the TVL monorepo
;;
;;; Code:

(require 'magit)

(define-suffix-command magit-push-and-submit ()
  (interactive)
  (magit-push-refspecs
   "origin" "HEAD:refs/for/master%l=Code-Review+2,publish-comments,submit"
   nil))

(transient-append-suffix
  #'magit-push
  ["r"]

  (list "P" "Push and submit to gerrit" #'magit-push-and-submit))

(provide 'tvl)
;;; tvl.el ends here
