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

(use-package evil
  :init
  (setq evil-want-integration nil)
  (general-evil-setup)
  :config
  ;; ensure [_-] are part of \w character set
  ;; Note: there must be a better way to accomplish this.
  (progn
    ;; underscore
    (modify-syntax-entry ?_ "w" sh-mode-syntax-table)

    ;; dash
    (modify-syntax-entry ?- "w" sh-mode-syntax-table)
    (modify-syntax-entry ?- "w" emacs-lisp-mode-syntax-table))
  (general-mmap
    :keymaps 'override
    "RET" #'evil-goto-line
    "H"   #'evil-first-non-blank
    "L"   #'evil-end-of-line
    "-"   #'dired-jump
    "sl"  #'wpc/evil-window-vsplit-right
    "sh"  #'evil-window-vsplit
    "sk"  #'evil-window-split
    "sj"  #'wpc/evil-window-split-down)
  (general-nmap
    :keymaps 'override
    "gd"  #'xref-find-definitions)
  (general-unbind 'motion "M-." "C-p")
  (general-unbind 'normal "s"   "M-.")
  (general-unbind 'insert "C-d" "C-a" "C-e" "C-n" "C-p" "C-k")
  (setq evil-symbol-word-search t)
  (evil-mode 1))

;; evil keybindings
(use-package evil-collection
  :after (evil)
  :config
  (evil-collection-init))

;; expose a leader key
(use-package evil-leader
  :after (evil counsel)
  :config
  (global-evil-leader-mode)
  (evil-leader/set-leader "<SPC>")
  ;; global
  (evil-leader/set-key
    "i"  #'counsel-semantic-or-imenu
    "j"  #'jump-to-register
    "h"  #'help
    "a"  #'wpc/toggle-terminal
    "p"  #'flycheck-previous-error
    "P"  #'counsel-git-grep
    "f"  #'wpc/find-file
    "n"  #'flycheck-next-error
    "N"  #'smerge-next
    "P"  #'smerge-prev
    "b"  #'ivy-switch-buffer
    "gs" #'magit-status

    "es" #'wpc/create-snippet
    "ev" (lambda () (interactive) (wpc/find-file-split "~/.config/nvim/init.vim"))
    "ee" (lambda () (interactive) (wpc/find-file-split "~/.emacs.d/init.el"))
    "ez" (lambda () (interactive) (wpc/find-file-split "~/.zshrc"))
    "ea" (lambda () (interactive) (wpc/find-file-split "~/aliases.zsh"))
    "ef" (lambda () (interactive) (wpc/find-file-split "~/functions.zsh"))
    "el" (lambda () (interactive) (wpc/find-file-split "~/variables.zsh"))
    "ex" (lambda () (interactive) (wpc/find-file-split "~/.Xresources"))
    "ei" (lambda () (interactive) (wpc/find-file-split "~/.config/i3/config.shared"))
    "em" (lambda () (interactive) (wpc/find-file-split "~/.tmux.conf"))

    "B"  #'magit-blame
    "w"  #'save-buffer
    "x"  #'evil-save-and-close
    "W"  #'save-all-buffers
    "r"  #'wpc/evil-replace-under-point
    ))

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

(defun wpc/ensure-kbds (_ignore)
  "Try to ensure that my keybindings retain priority over other minor modes."
  (unless (eq (caar minor-mode-map-alist) 'wpc/kbds-minor-mode)
    (let ((mykbds (assq 'wpc/kbds-minor-mode minor-mode-map-alist)))
      (assq-delete-all 'wpc/kbds-minor-mode minor-mode-map-alist)
      (add-to-list 'minor-mode-map-alist mykbds))))

;; Custom minor mode that ensures that my kbds are available no matter which
;; major or minor modes are active.
(add-hook 'after-load-functions #'wpc/ensure-kbds)

(defvar wpc/kbds
  (let ((map (make-sparse-keymap)))
    (bind-keys :map map
               ("M-q" . delete-window)
               ("C-x C-;" . comment-or-uncomment-region)
               ("C-x h" . help)
               ("<s-return>" . toggle-frame-fullscreen)
               ("<down-mouse-1>" . ffap-other-window)
               ("M-h"  . wpc/tmux-emacs-windmove-left)
               ("M-l"  . wpc/tmux-emacs-windmove-right)
               ("M-k"  . wpc/tmux-emacs-windmove-up)
               ("M-j"  . wpc/tmux-emacs-windmove-down)
               ("M--"  . wpc/evil-window-split-down)
               ("M-\\" . wpc/evil-window-vsplit-right)
               ("M-q"  . delete-window))
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
