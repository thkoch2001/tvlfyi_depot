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

(defconst dotfiles/whitelist
  '(("compton" . "~/.config/compton.conf")
    ("dotfiles" . "~/Dropbox/dotfiles/")
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
   (f-join "~/Dropbox/dotfiles/configs/shared/.emacs.d" name)))

(when dotfiles/install-kbds?
  (evil-leader/set-key "J" #'dotfiles/edit)
  (evil-leader/set-key "c" (lambda ()
                             (interactive)
                             (counsel-find-file "~/.config"))))

(provide 'dotfiles)
;;; dotfiles.el ends here
