(require 'wpc-package)

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
(require 'ssh)
(require 'clipboard)
(require 'battery)
(require 'bookmark)
(require 'keyboard)
(require 'irc)
(require 'email)
;; TODO: Consider renaming entr.el.
(require 'entr)
(require 'scrot)
(require 'timestring)

;; TODO: Remove path once published to MELPA.
;; TODO: How can I package this using Nix?
;; (require 'egg-timer "~/programming/egg-timer.el/egg-timer.el")

;; TODO: Reconcile kbd.el, keybindings.el, wpc-keybindings.el, keyboard.el.
(require 'wpc-keybindings)
(require 'keybindings)
(require 'window-manager)
(require 'wpc-ui)
(require 'wpc-dired)
(require 'wpc-org)
(require 'wpc-company)
;; TODO: Re-enable flycheck for all languages besides Elisp once I learn more
;; about the issue with the `emacs-lisp' `flycheck-checker'.
;; (require 'wpc-flycheck)
(require 'wpc-shell)
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
(require 'wpc-golang)
