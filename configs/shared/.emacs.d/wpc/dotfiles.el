;;; dotfiles.el --- Elisp to make dotfile management -*- lexical-binding: t -*-
;; Author: William Carroll <wpcarro@gmail.com>

;;; Commentary:
;; Quickly edit commonly used files.

;;; Code:

(require 'macros)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; API
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst dotfiles/install-kbds? t
  "When t, install the keybindings.")

(defconst dotfiles/whitelist
  '(("compton" . "~/.config/compton.conf")
    ("dotfiles" . "~/Dropbox/dotfiles/")
    ("functions" . "~/functions.zsh")
    ("aliases" . "~/aliases.zsh")
    ("variables" . "~/variables.zsh")
    ("Xresources" . "~/.Xresources.shared")
    ("tmux" . "~/.tmux.conf")
    ("i3" . "~/.config/i3/config") ;; TODO: Remove this one day.
    ("zshrc" . "~/.zshrc")
    ("configuration.nix" . "~/Dropbox/programming/nixify/configuration.nix")
    ("init.el" . "~/.emacs.d/init.el")
    ("init.vim" . "~/.config/nvim/init.vim"))
  "Dotfiles that I commonly edit.")

(defun dotfiles/edit ()
  "Select a dotfile from ivy and edit it in an Emacs buffer."
  (interactive)
  (ivy-read
   "Dotfile: "
   dotfiles/whitelist
   :action (>> cdr find-file)))

(when dotfiles/install-kbds?
  (evil-leader/set-key "J" #'dotfiles/edit))

(provide 'dotfiles)
;;; dotfiles.el ends here
