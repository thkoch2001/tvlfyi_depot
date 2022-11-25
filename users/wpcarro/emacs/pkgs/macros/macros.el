;;; macros.el --- Helpful variables for making my ELisp life more enjoyable -*- lexical-binding: t -*-

;; Author: William Carroll <wpcarro@gmail.com>
;; Version: 0.0.1
;; Package-Requires: ((emacs "24"))

;;; Commentary:
;; This file contains helpful variables that I use in my ELisp development.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Library
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro macros-enable (mode)
  "Helper for enabling `MODE'.
Useful in `add-hook' calls.  Some modes, like `linum-mode' need to be called as
`(linum-mode 1)', so `(add-hook mode #'linum-mode)' won't work."
  `#'(lambda nil (,mode 1)))

(defmacro macros-disable (mode)
  "Helper for disabling `MODE'.
Useful in `add-hook' calls."
  `#'(lambda nil (,mode -1)))

(defmacro macros-add-hook-before-save (mode f)
  "Register a hook, `F', for a mode, `MODE' more conveniently.
Usage: (macros-add-hook-before-save 'reason-mode-hook #'refmt-before-save)"
  `(add-hook ,mode
             (lambda ()
               (add-hook 'before-save-hook ,f))))

(defmacro macros-comment (&rest _)
  "Empty comment s-expresion where `BODY' is ignored."
  `nil)

(defmacro macros-support-file-extension (ext mode)
  "Register MODE to automatically load with files ending with EXT extension.
Usage: (macros-support-file-extension \"pb\" protobuf-mode)"
  (let ((extension (format "\\.%s\\'" ext)))
    `(add-to-list 'auto-mode-alist '(,extension . ,mode))))

(provide 'macros)
;;; macros.el ends here
