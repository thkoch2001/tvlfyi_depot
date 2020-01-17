(require 'wpc-package "~/.emacs.d/wpc/packages/wpc-package.el")
(require 'f)
(setq user-init-file (or load-file-name (buffer-file-name)))
(setq user-emacs-directory (f-dirname user-init-file))
;; TODO: Troubleshoot broken terminator.

;; load order is intentional
(require 'constants)
(require 'wpc-misc)

;; my libraries
(require 'functions)
(require 'prelude)
(require 'macros)
(require 'kaomoji)

;; Google
;; (require 'google-stuff)

;; Laptop XF-functionality
(require 'pulse-audio)
(require 'screen-brightness)

;; miscellaneous
(require 'clipboard)
(require 'battery)
(require 'dotfiles)
(require 'bookmark)
(require 'keyboard)
(require 'irc)
(require 'email)

(require 'wpc-keybindings)
(require 'window-manager)
(require 'wpc-ui)
(require 'wpc-dired)
(require 'wpc-terminal)
(require 'wpc-org)
(require 'wpc-company)
;; TODO: Re-enable flycheck for all languages besides Elisp once I learn more
;; about the issue with the `emacs-lisp' `flycheck-checker'.
;; (require 'wpc-flycheck)
(require 'wpc-shell)
(require 'wpc-docker)
(require 'wpc-lisp)
(require 'wpc-haskell)
(require 'wpc-reasonml)
;; (require 'wpc-ocaml)
(require 'wpc-elixir)
(require 'wpc-nix)
(require 'wpc-rust)
(require 'wpc-clojure)
(require 'wpc-python)
(require 'wpc-javascript)
(require 'wpc-java)
(require 'wpc-prolog)

(run-hooks 'after-init-hook)
