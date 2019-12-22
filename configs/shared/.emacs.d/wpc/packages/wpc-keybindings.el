;;; keybindings.el --- My Evil preferences -*- lexical-binding: t -*-
;; Author: William Carroll <wpcarro@gmail.com>

;;; Commentary:
;; This module hosts my Evil preferences
;;
;; Wish List:
;; - drop support for `evil-leader' library in favor of `general.el'
;; - restore support for concise (n <kbd> <function>) instead of `general-mmap'
;; - restore support for `general-unbind'

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This may be contraversial, but I never use the prefix key, and I'd prefer to
;; have to bound to the readline function that deletes the entire line.
(general-unbind "C-u")

(use-package evil
  :init
  ;; Should remove the warning messages on init.
  (setq evil-want-integration t)
  ;; TODO: Troubleshoot why this binding causes the following warning:
  ;; "Warning (evil-collection): `evil-want-keybinding' was set to nil but not
  ;; before loading evil."
  (setq evil-want-keybinding nil)
  (general-evil-setup)
  :config
  ;; Ensure that evil's command mode behaves with readline bindings.
  (general-define-key
   :keymaps 'evil-ex-completion-map
   "C-a" #'move-beginning-of-line
   "C-e" #'move-end-of-line
   "C-k" #'kill-line
   "C-u" #'evil-delete-whole-line
   "C-v" #'evil-paste-after
   "C-d" #'delete-char
   "C-f" #'forward-char
   "M-b" #'backward-word
   "M-f" #'forward-word
   "M-d" #'kill-word
   "M-DEL" #'backward-kill-word
   "C-b" #'backward-char)
  ;; TODO: Ensure all of my custom keybindings end up in a single map that is
  ;; easy to enable or disable.
  (general-mmap
    :keymaps 'override
    "RET" #'evil-goto-line
    "H"   #'evil-first-non-blank
    "L"   #'evil-end-of-line
    "_"   #'ranger
    "-"   #'dired-jump
    "sl"  #'wpc/evil-window-vsplit-right
    "sh"  #'evil-window-vsplit
    "sk"  #'evil-window-split
    "sj"  #'wpc/evil-window-split-down)
  (general-nmap
    :keymaps 'override
    "gd" #'xref-find-definitions
    ;; Wrapping `xref-find-references' in the `let' binding to prevent xref from
    ;; prompting.  There are other ways to handle this variable, such as setting
    ;; it globally with `setq' or buffer-locally with `setq-local'.  For now, I
    ;; prefer setting it with `let', which should bind it in the dynamic scope
    ;; for the duration of the `xref-find-references' function call.
    "gx" (lambda ()
           (interactive)
           (let ((xref-prompt-for-identifier nil))
             (call-interactively #'xref-find-references))))
  (general-unbind 'motion "M-." "C-p")
  (general-unbind 'normal "s"   "M-." "C-p" "C-n")
  (general-unbind 'insert "C-v" "C-d" "C-a" "C-e" "C-n" "C-p" "C-k")
  (setq evil-symbol-word-search t)
  (evil-mode 1))

;; TODO: Write `evil-collection' KBDs for `refine'.
;; evil keybindings
(use-package evil-collection
  :after (evil)
  :config
  (evil-collection-init))

;; `evil-collection' does not support `magit', and the preferred way to get evil
;; kbds for magit is with `evil-magit'.
(use-package evil-magit)

;; expose a leader key
(use-package evil-leader
  :after (evil)
  :config
  (global-evil-leader-mode 1)
  (evil-leader/set-leader "<SPC>")
  (evil-leader/set-key
    "i"   #'counsel-semantic-or-imenu
    "I"   #'ibuffer
    "hk"  #'helpful-callable
    "hf"  #'helpful-function
    "hm"  #'helpful-macro
    "hc"  #'helpful-command
    "hk"  #'helpful-key
    "hv"  #'helpful-variable
    "hp"  #'helpful-at-point
    "s"   #'flyspell-mode
    "S"   #'sort-lines
    "a"   #'wpc-terminal/toggle
    "="   #'align
    "p"   #'flycheck-previous-error
    "f"   #'wpc/find-file
    "n"   #'flycheck-next-error
    "N"   #'smerge-next
    "b"   #'ivy-switch-buffer
    "W"   #'balance-windows
    "gs"  #'magit-status
    "E"   #'refine

    "es" #'wpc/create-snippet
    ;; TODO: Replace with `macros/ilambda' when that is working again.
    "ev" (lambda () (interactive) (wpc/find-file-split "~/.config/nvim/init.vim"))
    "ee" (lambda () (interactive) (wpc/find-file-split "~/.emacs.d/init.el"))
    "ez" (lambda () (interactive) (wpc/find-file-split "~/.zshrc"))
    "ea" (lambda () (interactive) (wpc/find-file-split "~/aliases.zsh"))
    "ef" (lambda () (interactive) (wpc/find-file-split "~/functions.zsh"))
    "el" (lambda () (interactive) (wpc/find-file-split "~/variables.zsh"))
    "ex" (lambda () (interactive) (wpc/find-file-split "~/.Xresources"))
    "ei" (lambda () (interactive) (wpc/find-file-split "~/.config/i3/config.shared"))
    "em" (lambda () (interactive) (wpc/find-file-split "~/.tmux.conf"))

    "l"  #'locate
    "L"  #'list-packages
    "B"  #'magit-blame
    "w"  #'save-buffer
    "r"  #'wpc/evil-replace-under-point
    "R"  #'deadgrep))

;; create comments easily
(use-package evil-commentary
  :after (evil)
  :config
  (evil-commentary-mode))

;; evil surround
(use-package evil-surround
  :after (evil)
  :config
  (global-evil-surround-mode 1))

;; I expect in insert mode:
;; C-a: beginning-of-line
;; C-e: end-of-line
;; C-b: backwards-char
;; C-f: forwards-char

;; TODO: Move these KBD constants to kbd.el.

(defconst wpc/up-kbds
  '("C-p" "C-k" "<backtab>" "<up>")
  "The keybindings that I expect to work for moving upwards in lists.")

(defconst wpc/down-kbds
  '("C-n" "C-j" "<tab>" "<down>")
  "The keybindings that I expect to work for moving downwards in lists.")

(defconst wpc/left-kbds
  '("C-b" "<left>")
  "The keybindings that I expect to move leftwards in insert-like modes.")

(defconst wpc/right-kbds
  '("C-f" "<right>")
  "The keybindings that I expect to move rightwards in insert-like modes.")

(defun wpc/ensure-kbds (_ignore)
  "Try to ensure that my keybindings retain priority over other minor modes."
  (unless (eq (caar minor-mode-map-alist) 'wpc/kbds-minor-mode)
    (let ((mykbds (assq 'wpc/kbds-minor-mode minor-mode-map-alist)))
      (assq-delete-all 'wpc/kbds-minor-mode minor-mode-map-alist)
      (add-to-list 'minor-mode-map-alist mykbds))))

;; Custom minor mode that ensures that my kbds are available no matter which
;; major or minor modes are active.
(add-hook 'after-load-functions #'wpc/ensure-kbds)

;; TODO: Prefer using general and 'override maps to implement this.
(defvar wpc/kbds
  (let ((map (make-sparse-keymap)))
    (bind-keys :map map
               ("M-q"            . delete-window)
               ("<s-return>"     . toggle-frame-fullscreen)
               ("M-h"            . windmove-left)
               ("M-l"            . windmove-right)
               ("M-k"            . windmove-up)
               ("M-j"            . windmove-down)
               ("M-q"            . delete-window))
    map)
  "William Carroll's keybindings that should have the highest precedence.")

(define-minor-mode wpc/kbds-minor-mode
  "A minor mode so that my key settings override annoying major modes."
  :init-value t
  :lighter " wpc/kbds"
  :keymap wpc/kbds)

;; allow jk to escape
(use-package key-chord
  :after (evil)
  :config
  (key-chord-mode 1)
  (key-chord-define evil-insert-state-map "jk" 'evil-normal-state))

(provide 'wpc-keybindings)
;;; wpc-keybindings.el ends here
