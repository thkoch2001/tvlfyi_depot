;;; term-switcher.el --- Easily switch between open vterms
;;
;; Copyright (C) 2019-2020 Google Inc.
;; Copyright (C) 2021-2023 The TVL Authors
;;
;; Author: Vincent Ambo <tazjin@tvl.su>
;; Version: 1.1
;; Package-Requires: (ivy s vterm)
;;
;;; Commentary:
;;
;; This package adds a function that lets users quickly switch between
;; different open vterms via ivy.

(require 'seq)
(require 'ivy)
(require 's)
(require 'vterm)

(defgroup term-switcher nil
  "Customization options `term-switcher'.")

(defcustom term-switcher-buffer-prefix "vterm<"
  "String prefix for vterm terminal buffers. For example, if you
  set your titles to match `vterm<...>' a useful prefix might be
  `vterm<'."
  :type '(string)
  :group 'term-switcher)

(defun ts/open-or-create-vterm (buffer)
  "Switch to the terminal in BUFFER, or create a new one if buffer is nil."
  (if buffer
      (switch-to-buffer buffer)
    ;; Don't open semi-broken vterms over tramp.
    (if (file-remote-p default-directory)
        (let ((default-directory "~"))
          (vterm))
      (vterm))))

(defun ts/is-vterm-buffer (buffer)
  "Determine whether BUFFER runs a vterm."
  (equal 'vterm-mode (buffer-local-value 'major-mode buffer)))

(defun ts/switch-to-terminal ()
  "Switch to an existing vterm buffer or create a new one."

  (interactive)
  (let ((terms (seq-map (lambda (b) (cons (buffer-name b) b))
                        (seq-filter #'ts/is-vterm-buffer (buffer-list)))))
    (if terms
        (ivy-read "Switch to vterm: "
                  (cons "New vterm" (seq-map #'car terms))
                  :caller 'ts/switch-to-terminal
                  :preselect (s-concat "^" term-switcher-buffer-prefix)
                  :require-match t
                  :action (lambda (match)
                            (ts/open-or-create-vterm (cdr (assoc match terms)))))
      (vterm))))

(provide 'term-switcher)
