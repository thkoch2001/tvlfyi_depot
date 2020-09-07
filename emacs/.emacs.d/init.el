;; This suppresses the warning about Emacs 27 deprecating the cl library in
;; favor of cl-lib.
;; See this thread for more details:
;; https://github.com/kiwanami/emacs-epc/issues/35
(setq byte-compile-warnings '(cl-functions))

;; load order is intentional
(require 'wpc-package)
(require 'wpc-misc)
(require 'ssh)
(require 'keyboard)
(require 'irc)
(require 'email)
(require 'keybindings)
(require 'window-manager)
(require 'wpc-ui)
(require 'wpc-dired)
(require 'wpc-org)
(require 'wpc-company)
(require 'wpc-shell)
(require 'wpc-lisp)
(require 'wpc-haskell)
(require 'wpc-elixir)
(require 'wpc-nix)
(require 'wpc-rust)
(require 'wpc-clojure)
(require 'wpc-python)
(require 'wpc-javascript)
(require 'wpc-prolog)
(require 'wpc-golang)
