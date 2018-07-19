;;; keybindings.el --- My Evil preferences -*- lexical-binding: t -*-
;; Author: William Carroll <wpcarro@gmail.com>

;;; Commentary:
;; This module hosts my Evil preferences

;;; Code:

(quelpa
 '(general
   :repo "noctuid/general.el"
   :fetcher github))
(general-evil-setup t)

(defun wpc/goto-definition ()
  "Support a goto-definition that dispatches off of the `major-mode'."
  (interactive)
  (require 'flow-minor-mode)
  (cond ((eq major-mode 'rjsx-mode)
         (flow-minor-jump-to-definition))
        ((eq major-mode 'haskell-mode)
         (intero-goto-definition))
        (t
         (evil-goto-definition))))

;; vim...
(use-package evil
  :general
  (m
   "RET" 'evil-goto-line
   "H"   'evil-first-non-blank
   "L"   'evil-end-of-line
   "-"   'dired-jump
   "sl"  'wpc/evil-window-vsplit-right
   "sh"  'evil-window-vsplit
   "sk"  'evil-window-split
   "sj"  'wpc/evil-window-split-down
   "sj"  'wpc/evil-window-split-down)
  (n
   "gd"  'wpc/goto-definition)
  (general-unbind m "M-." "C-p")
  (general-unbind n "s" "M-.")
  (general-unbind i "C-d" "C-a" "C-e" "C-n" "C-p" "C-k")
  (evil-ex-map
   "M-p" 'previous-complete-history-element
   "M-n" 'next-complete-history-element)
  :init
  (setq evil-want-integration nil)
  :config
  (setq evil-symbol-word-search t)
  (evil-mode 1))

;; evil keybindings
(use-package evil-collection
  :after evil
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
    "p"  #'counsel-git-grep
    "P"  #'counsel-git-grep
    "f"  #'wpc/find-file
    "N"  #'smerge-next
    "P"  #'smerge-prev
    "s"  #'slack-send-code-snippet
    "S"  #'slack-select-unread-rooms
    "b"  #'ivy-switch-buffer
    "gs" #'magit-status
    "es" #'wpc/create-snippet
    "ev" #'wpc/edit-init-el
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

;; Custom minor mode that ensures that my kbds are available no matter which major
;; or minor modes are active.
(add-hook 'after-load-functions #'ensure-william-carroll-kbds)

(defun ensure-william-carroll-kbds (_ignore)
  "Try to ensure that my keybindings retain priority over other minor modes."
  (unless (eq (caar minor-mode-map-alist) 'wpc/kbds-minor-mode)
    (let ((mykbds (assq 'wpc/kbds-minor-mode minor-mode-map-alist)))
      (assq-delete-all 'wpc/kbds-minor-mode minor-mode-map-alist)
      (add-to-list 'minor-mode-map-alist mykbds))))

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
               ("M--"  . split-window-below)
               ("M-\\" . split-window-right)
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
