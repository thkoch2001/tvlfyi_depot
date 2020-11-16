;;; ../code/depot/users/glittershark/emacs.d/vterm.el -*- lexical-binding: t; -*-

(defun require-vterm ()
  (add-to-list
   'load-path
   (concat
    (s-trim
     (shell-command-to-string
      "nix-build --no-out-link ~/code/depot -A third_party.emacs.vterm"))
    "/share/emacs/site-lisp/elpa/vterm-20200515.1412"))
  (require 'vterm))

(defun +grfn/vterm-setup ()
  (hide-mode-line-mode)
  (setq-local evil-collection-vterm-send-escape-to-vterm-p t))

(add-hook 'vterm-mode-hook #'+grfn/vterm-setup)

(map! (:map vterm-mode-map
       "<C-escape>" #'evil-normal-state))

(comment
 (require-vterm)
 )
