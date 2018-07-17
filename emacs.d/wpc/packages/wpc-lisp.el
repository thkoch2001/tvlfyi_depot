;;; lisp.el --- Generic LISP preferences -*- lexical-binding: t -*-
;; Author: William Carroll <wpcarro@gmail.com>

;;; Commentary:
;; This hosts things like Paredit settings

;;; Code:

(defconst wpc/lisp-mode-hooks
  '(emacs-lisp-mode-hook
    clojure-mode-hook
    clojurescript-mode-hook))

;; Elisp
(use-package elisp-slime-nav
  :ghook
  'emacs-lisp-mode
  'ielm-mode)

;; Here is some of the thinking behind some of the keybindings:
;;
;; slurp    s
;; barf     S
;; forward  )
;; backward (
;;
;; Known concession: s and S eclipse Vim bindings. There is a precedent already
;; for eclipsing the s binding for window splitting. Shift-s feel appropriate
;; for barfing, since eclisping the b KBD feels less acceptable than eclisping
;; the s KBD.

;; paredit LISP editing
(use-package paredit
  :general
  (general-unbind paredit-mode-map "C-j" "M-q")
  (n paredit-mode-map
     "s)" 'paredit-forward-slurp-sexp
     "s(" 'paredit-backward-slurp-sexp
     "S)" 'paredit-forward-barf-sexp
     "S(" 'paredit-backward-barf-sexp
     "gr" 'paredit-raise-sexp)
  :ghook (wpc/lisp-mode-hooks #'enable-paredit-mode))

(provide 'wpc-lisp)
;;; lisp.el ends here
