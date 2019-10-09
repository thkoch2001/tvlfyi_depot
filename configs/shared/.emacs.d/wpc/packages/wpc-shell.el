;;; wpc-shell.el --- POSIX Shell scripting support -*- lexical-binding: t -*-
;; Author: William Carroll <wpcarro@gmail.com>

;;; Commentary:
;; Helpers for my shell scripting. Includes bash, zsh, etc.

;;; Code:

(use-package flymake-shellcheck
  :commands flymake-shellcheck-load
  :init
  (add-hook 'sh-mode-hook #'flymake-shellcheck-load))

(provide 'wpc-shell)
;;; wpc-shell.el ends here
