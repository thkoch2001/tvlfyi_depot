;;; google-tooling.el --- Better access to Google tooling -*- lexical-binding: t -*-
;; Author: William Carroll <wpcarro@gmail.com>

;;; Commentary:

;; First, I must opine.  Feel free to skip this section.  In general, it seems
;; that the average programmer's workflow suffer from what economists call
;; "inelastic demand".  This means that any increase in price for something
;; sends the demand plummeting.  Another way of phrasing this is that
;; programmers are "price sensitive" when it comes to adopting new workflows.
;;
;; For us, any deviation from our "established" workflow feels costly.  This
;; makes sense to me because programming is already taxing, so any additional
;; taxation can feel unbearable.  Until programming changes dramatically, and we
;; relieve our dependence on files and text for modeling complex applications,
;; this inelastic demand will remain the status quo.  Therefore, it's critical
;; to reduce the price of experimenting with new tools such that new, superior
;; habits may form.  In this vain, this module attempts to surface "luxury
;; tools" (i.e. dependency pruners, code linters, code formatters) via Emacs to
;; reduce the price of experimenting with them.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dependencies
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'macros)

;; TODO: Figure out whether or not to integrate with google-emacs.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Library
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst google-tooling/tools
  '(("Depana" . "depana")
    ("Build cleaner" . "build_cleaner")
    ("Java formatter" . "google-java-format")
    ("Proto formatter" . "clang-format"))
  "Mapping of names of tools to the names of the executables that run them.")

(use-package protobuf-mode
  :config
  (macros/support-file-extension "pb" protobuf-mode))

;; TODO: Call blaze build, use Counsel to select an action, run that action on
;; the nearest BUILD file.

;; TODO: Call build-cleaner on the nearest BUILD file.

(provide 'google-tooling)
;;; google-tooling.el ends here
