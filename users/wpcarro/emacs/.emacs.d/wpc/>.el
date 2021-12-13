;;; >.el --- Small utility functions -*- lexical-binding: t -*-

;; Author: William Carroll <wpcarro@gmail.com>
;; Version: 0.0.1
;; URL: https://git.wpcarro.dev/wpcarro/briefcase
;; Package-Requires: ((emacs "24"))

;;; Commentary:
;; Originally I stored the `>>` macro in macros.el, but after setting up linting
;; for my Elisp in CI, `>>` failed because it didn't have the `macros-`
;; namespace.  I created this module to establish a `>-` namespace under which I
;; can store some utilities that would be best kept without a cumbersome
;; namespace.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Library
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro >-> (&rest forms)
  "Compose a new, point-free function by composing FORMS together."
  (let ((sym (gensym)))
    `(lambda (,sym)
       (->> ,sym ,@forms))))


(provide '>)
;;; >.el ends here
