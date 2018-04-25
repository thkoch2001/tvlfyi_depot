(require 'wpc-package "~/.emacs.d/wpc/packages/wpc-package.el")

;; third-party libraries
(use-package dash)
(use-package s)
(use-package f)

;; my libraries
(require 'variables)
(require 'functions)
(require 'string-functions)
(require 'macros)
(require 'casing)

(require 'wpc-ui)
(require 'wpc-keybindings)
(require 'wpc-misc)
(require 'wpc-dired)
(require 'wpc-terminal)
(require 'wpc-org)
(require 'wpc-slack)
(require 'wpc-company)
(require 'wpc-flycheck)
(require 'wpc-git)
(require 'wpc-docker)
(require 'wpc-lisp)
(require 'wpc-haskell)
(require 'wpc-clojure)
(require 'wpc-javascript)
