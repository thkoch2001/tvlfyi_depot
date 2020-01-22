;;; lisp.el --- Generic LISP preferences -*- lexical-binding: t -*-
;; Author: William Carroll <wpcarro@gmail.com>

;; parent (up)
;; child (down)
;; prev-sibling (left)
;; next-sibling (right)

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dependencies
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'general)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst wpc/lisp-mode-hooks
  '(lisp-mode-hook
    emacs-lisp-mode-hook
    clojure-mode-hook
    clojurescript-mode-hook
    racket-mode-hook)
  "List of LISP modes.")

(use-package sly
  :config
  (setq inferior-lisp-program "sbcl")
  (general-define-key
   :keymaps 'sly-mode-map
   :states '(normal)
   :prefix "<SPC>"
   "x" #'sly-eval-defun
   "X" #'sly-eval-buffer
   "d" #'sly-describe-symbol))

(use-package rainbow-delimiters
  :config
  (general-add-hook wpc/lisp-mode-hooks #'rainbow-delimiters-mode))

(use-package racket-mode
  :config
  (general-define-key
   :keymaps 'racket-mode-map
   :states 'normal
   :prefix "<SPC>"
   "x" #'racket-send-definition
   "X" #'racket-run
   "d" #'racket-describe)
  (setq racket-program "~/.nix-profile/bin/racket"))

(use-package lispyville
  :init
  (defconst lispyville-key-themes
    '(c-w
      operators
      text-objects
      ;; Disabling this because I don't enjoy the way it moves around comments.
      ;; atom-motions
      prettify
      commentary
      slurp/barf-cp
      wrap
      additional
      additional-insert
      additional-wrap
      escape)
    "All available key-themes in Lispyville.")
  :config
  (general-add-hook wpc/lisp-mode-hooks #'lispyville-mode)
  (lispyville-set-key-theme lispyville-key-themes)
  (progn
    ;;
    (general-define-key
     :keymaps 'lispyville-mode-map
     :states 'motion
     ;; first unbind
     "M-h" nil
     "M-l" nil)
    (general-define-key
     :keymaps 'lispyville-mode-map
     :states 'normal
     ;; first unbind
     "M-j" nil
     "M-k" nil
     ;; second rebind
     ;; TODO: Rebind to something that doesn't conflict with window resizing.
     "C-s-h" #'lispyville-drag-backward
     "C-s-l" #'lispyville-drag-forward
     "C-s-e" #'lispyville-end-of-defun
     "C-s-a" #'lispyville-beginning-of-defun)))

;; deletes all bindings of f->kbd
;; binds kbd->
;; (kbd/bind-function->key
;;  :keymap 'lispyville-mode-map
;;  :states 'motion
;;  #'lispyville-drag-backward "H")

;; Elisp
(use-package elisp-slime-nav
  :config
  (general-add-hook 'emacs-lisp-mode #'ielm-mode))

(general-define-key
 :keymaps 'emacs-lisp-mode-map
 :prefix "<SPC>"
 :states 'normal
 "x" #'eval-defun
 "X" #'eval-buffer
 "d" (lambda ()
       (interactive)
       (with-current-buffer (current-buffer)
         (helpful-function (symbol-at-point)))))

(provide 'wpc-lisp)
;;; wpc-lisp.el ends here
