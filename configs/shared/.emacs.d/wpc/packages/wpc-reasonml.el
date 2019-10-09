;;; wpc-reasonml.el --- My ReasonML preferences -*- lexical-binding: t -*-
;; Author: William Carroll <wpcarro@gmail.com>

;;; Commentary:
;; Tooling support for ReasonML development.
;;
;; Dependencies:
;; - `opam install tuareg`
;; - `opam install merlin`
;; - `opam install user-setup`
;; - `opam install ocamlformat`

;;; Code:

;; ReasonML configuration
(use-package reason-mode
  :config
  (add-hook-before-save 'reason-mode-hook #'refmt-before-save))

;; ReasonML LSP configuration
(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection (f-full "~/programming/dependencies/reason-language-server"))
                  :major-modes '(reason-mode)
                  :notification-handlers (ht ("client/registerCapability" 'ignore))
                  :priority 1
                  :server-id 'reason-ls))

(provide 'wpc-reasonml)
;;; wpc-reasonml.el ends here
