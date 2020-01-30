;;; keymap.el --- Working with Elisp keymaps -*- lexical-binding: t -*-
;; Author: William Carroll <wpcarro@gmail.com>

;;; Commentary:
;; Very much a work-in-progress.

;;; Code:

(require 'macros)
(require 'symbol)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Library
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun keymap/pretty-print (x)
  "Pretty prints `X'."
  ;; TODO: Work-in-progress
  (s-concat "\\{" (symbol/to-string x) "}"))

(macros/comment
 (keymap/pretty-print lispyville-mode-map))

(provide 'keymap)
;;; keymap.el ends here
