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

(prelude/assert
 (prelude/executable-exists? "opam"))

(defvar opam-installs "~/.opam/4.08.0/share/emacs/site-lisp"
  "Path to the Ocaml PAckage Manager installations.")

(defvar opam-user-setup "~/.emacs.d/opam-user-setup.el"
  "File for the OPAM Emacs integration.")

(prelude/assert
 (f-file? opam-user-setup))

(prelude/assert
 (f-dir? opam-installs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package tuareg
  :config
  (add-hook-before-save 'tuareg-mode-hook #'ocamlformat-before-save))

;; ocamlformat
(require 'opam-user-setup "~/.emacs.d/opam-user-setup.el")
(require 'ocamlformat)
(add-to-list 'load-path opam-installs)

(provide 'wpc-ocaml)
;;; wpc-ocaml.el ends here
