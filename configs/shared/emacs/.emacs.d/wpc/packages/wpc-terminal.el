;;; terminal.el --- My cobbled together terminal -*- lexical-binding: t -*-
;; Author: William Carroll <wpcarro@gmail.com>

;;; Commentary:
;; My attempts at creating a sane Emacs terminal
;;
;; This module previously contained more logic, which has since been stripped.
;;
;; If the variable `explicit-shell-file-name' is `nil', Emacs will use the value
;; for the $SHELL environment variable.  When running on NixOS, since binaries
;; like `zsh' won't be available at `/bin/zsh' or other common places, we need
;; to ensure that `explicit-shell-file-name' remain `nil'.
;;
;; Wish List:
;; - prevent Emacs from asking: "Run program: /run/current-system/sw/bin/zsh"

;;; Code:

(setq wpc/terminal-name "wpc/terminal")

(provide 'wpc-terminal)
;;; wpc-terminal.el ends here
