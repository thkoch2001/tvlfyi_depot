;;; ~/code/depot/users/glittershark/emacs.d/lisp.el -*- lexical-binding: t; -*-

(defun grfn/sly-panettone ()
  (interactive)
  (sly
   (concat
    (s-trim
     (shell-command-to-string
      "nix-build -o sbcl -E 'with import ~/code/depot {}; nix.buildLisp.sbclWith [web.panettone]'"))
    "/bin/sbcl")))

(defun grfn/setup-lisp ()
  (interactive)
  (unless paxedit-mode (paxedit-mode 1))
  (rainbow-delimiters-mode)
  (flycheck-mode -1))

(add-hook 'common-lisp-lisp-mode-hook #'grfn/setup-lisp)

(defun sly-run-tests ()
  (interactive)
  ;; TODO: handle other test frameworks
  (let ((orig-window (get-buffer-window)))
    (sly-eval '(fiveam:run!))
    (funcall-interactively #'sly-mrepl-sync)
    (select-window orig-window)))

(map!
 (:map sly-mode-map
  :n "g \\" #'sly-mrepl-sync
  :n "g d" #'sly-edit-definition
  :n "K" #'sly-documentation
  :n "g SPC" #'sly-compile-and-load-file
  :n "g RET" #'sly-run-tests)

 (:map sly-mrepl-mode-map
  "C-k" #'sly-mrepl-previous-prompt
  "C-r" #'isearch-backward))
