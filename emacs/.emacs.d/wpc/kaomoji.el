;;; kaomoji.el --- Supporting kaomoji usage -*- lexical-binding: t -*-
;; Author: William Carroll <wpcarro@gmail.com>

;;; Commentary:
;; Simple keyboards like this make life a bit better.

;;; Code:

(defvar kaomoji/install-kbds?
  nil
  "Set to t if you'd like the keybindings to be installed.")

(defconst kaomoji/symbols '(("Joy" . "(⌒‿⌒)")
                    ("Love" . "(ღ˘⌣˘ღ)")
                    ("Sympathy" . "ヽ(~_~(・_・ )ゝ")
                    ("Dissatisfaction" . "(＞﹏＜)")
                    ("Anger" . "ヽ(‵﹏´)ノ")
                    ("Hugging" . "(づ￣ ³￣)づ")
                    ("Hiding" . "┬┴┬┴┤( ͡° ͜ʖ├┬┴┬┴")
                    ("Sleeping" . "(－_－) zzZ")
                    ("Embarrassed" . "(×﹏×)")
                    ("Shrug" . "ヽ(ー_ー )ノ"))
  "Alist of human-readable emotions to the kaomoji.")

;; TODO: Consider supporting a hydra for these.

(defun kaomoji/select ()
  "Interactively select a kaomoji and copy it to the clipboard."
  (interactive)
  (ivy-read
   "Select a kaomoji: "
   kaomoji/symbols
   :action (lambda (entry)
             (kill-new (cdr entry))
             (alert "Copied to clipboard!"))))

;; TODO: Define Hydra for all custom keyboards.
;; TODO: Define a better keybinding in a different keymap.
(when kaomoji/install-kbds?
  (general-define-key
   :keymaps 'global
   "M-k" #'kaomoji/select))

(provide 'kaomoji)
;;; kaomoji.el ends here
