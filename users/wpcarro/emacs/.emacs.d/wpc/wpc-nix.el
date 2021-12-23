;;; wpc-nix.el --- Nix support -*- lexical-binding: t -*-

;; Author: William Carroll <wpcarro@gmail.com>
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1"))

;;; Commentary:
;; Configuration to support working with Nix.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dependencies
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'tvl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Library
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package nix-mode
  :mode "\\.nix\\'")

(defun wpc-nix-rebuild-emacs ()
  "Use nix-env to rebuild wpcarros-emacs."
  (interactive)
  (let* ((pname (format "nix-env -iA users.wpcarro.emacs.nixos"))
         (bname (format "*%s*" pname)))
    (start-process pname bname
                   "nix-env"
                   "-f" tvl-depot-path
                   "-iA" "users.wpcarro.emacs.nixos")
    (display-buffer bname)))

(provide 'wpc-nix)
;;; wpc-nix.el ends here
