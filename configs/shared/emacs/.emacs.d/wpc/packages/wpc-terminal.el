;;; terminal.el --- My cobbled together terminal -*- lexical-binding: t -*-
;; Author: William Carroll <wpcarro@gmail.com>

;;; Commentary:
;; My attempts at creating a sane Emacs terminal

;;; Code:

(setq wpc/terminal-name "wpc/terminal")

;; 256 color support in term (instead of 8)
(use-package xterm-color)

(use-package term
  :config
  (setq explicit-shell-file-name "/bin/zsh"))

(provide 'wpc-terminal)
;;; terminal.el ends here
