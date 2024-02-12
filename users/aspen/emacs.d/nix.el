;;; -*- lexical-binding: t; -*-

(defun nix-buffer-type ()
  "Returns:

'home-manager, if the current buffer is a home-manager module
'nixos, if the current buffer is a nixos module
nil, if none of the above are the case"
  (when buffer-file-name
    (pcase buffer-file-name
      ((rx (0+ nonl) "system/home" (0+ nonl) ".nix" eos)
       'home-manager)
      ((rx (0+ nonl) "system/system" (0+ nonl) ".nix" eos)
       'nixos))))

(defun set-nix-compile-command ()
  "Set the compile command for the current buffer based on the type of nix
buffer it is, per `nix-buffer-type'"
  (interactive)
  (when-let ((btype (nix-buffer-type)))
    (setq-local
     compile-command
     (case btype
       ('home-manager "home-manager switch")
       ('nixos "sudo nixos-rebuild switch")))))

(add-hook 'nix-mode-hook #'set-nix-compile-command)

(map! (:map nix-mode-map
       (:n "g SPC" #'compile)))
