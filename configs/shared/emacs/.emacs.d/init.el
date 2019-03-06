(require 'wpc-package "~/.emacs.d/wpc/packages/wpc-package.el")

;; load order is intentional
(require 'variables)
(require 'wpc-misc)

;; my libraries
(require 'functions)
(require 'string-functions)
(require 'macros)
(require 'casing)

(require 'wpc-ui)
(require 'wpc-keybindings)
(require 'wpc-dired)
(require 'wpc-terminal)
(require 'wpc-org)
(require 'wpc-company)
(require 'wpc-flycheck)
(require 'wpc-docker)
(require 'wpc-lisp)
(require 'wpc-haskell)
(require 'wpc-elixir)
(require 'wpc-nix)
(require 'wpc-clojure)
(require 'wpc-javascript)
