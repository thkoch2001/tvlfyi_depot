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

;; paredit LISP editing
(use-package paredit
  :general
  (general-unbind paredit-mode-map "C-j" "M-q")
  (n paredit-mode-map
     ">)" 'paredit-forward-slurp-sexp
     "<(" 'paredit-backward-slurp-sexp
     "<)" 'paredit-forward-barf-sexp
     ">(" 'paredit-backward-barf-sexp
     ">e" 'paredit-move-forward
     "<e" 'paredit-move-backward
     ">f" 'paredit-move-backward
     "<f" 'paredit-move-backward
     "go" 'paredit-raise-sexp)
  :ghook (wpc/lisp-mode-hooks #'enable-paredit-mode))

(provide 'wpc-lisp)
;;; lisp.el ends here
