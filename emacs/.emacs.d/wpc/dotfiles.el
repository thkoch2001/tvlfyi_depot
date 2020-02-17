;;; dotfiles.el --- Elisp to make dotfile management -*- lexical-binding: t -*-
;; Author: William Carroll <wpcarro@gmail.com>

;;; Commentary:
;; Quickly edit commonly used files.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dependencies
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'macros)
(require 'f)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; API
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst dotfiles/install-kbds? t
  "When t, install the keybindings.")

(defconst dotfiles/directory (getenv "BRIEFCASE")
  "The root directory of my configuration files.")

(defconst dotfiles/whitelist
  `(("dotfiles" . ,dotfiles/directory)
    ("functions" . "~/functions.zsh")
    ("aliases" . "~/aliases.zsh")
    ("variables" . "~/variables.zsh")
    ("Xresources" . "~/.Xresources.shared")
    ("xsession" . "~/.xsessionrc.shared")
    ("tmux" . "~/.tmux.conf")
    ("zshrc" . "~/.zshrc")
    ("config.fish" . "~/.config/fish/config.fish")
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

(defun dotfiles/find-emacs-file (name)
  "Call `find-file' on NAME located in dotfiles's emacs.d directory."
  (find-file
   (f-join dotfiles/directory "emacs/.emacs.d" name)))

(provide 'dotfiles)
;;; dotfiles.el ends here
