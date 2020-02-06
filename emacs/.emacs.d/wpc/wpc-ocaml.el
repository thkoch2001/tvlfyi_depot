;;; wpc-ocaml.el --- My OCaml preferences -*- lexical-binding: t -*-
;; Author: William Carroll <wpcarro@gmail.com>

;;; Commentary:
;; Tooling support for OCaml development.
;;
;; Dependencies:
;; - `opam install tuareg`
;; - `opam install merlin`
;; - `opam install user-setup && opam user-setup install`
;; - `opam install ocamlformat`

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dependencies
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'prelude)
(require 'f)

(defvar opam-user-setup "~/.emacs.d/opam-user-setup.el"
  "File for the OPAM Emacs integration.")

(prelude/assert (f-file? opam-user-setup))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package tuareg
  :config
  (add-hook-before-save 'tuareg-mode-hook #'ocamlformat-before-save))

;; ocamlformat
(require 'opam-user-setup "~/.emacs.d/opam-user-setup.el")
(require 'ocamlformat)

(provide 'wpc-ocaml)
;;; wpc-ocaml.el ends here
