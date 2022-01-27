;;; keybindings.el --- Centralizing my keybindings -*- lexical-binding: t -*-

;; Author: William Carroll <wpcarro@gmail.com>
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1"))

;;; Commentary:
;; Attempting to centralize my keybindings to simplify my configuration.
;;
;; I have some expectations about my keybindings.  Here are some of those
;; defined:
;; - In insert mode:
;;   - C-a: beginning-of-line
;;   - C-e: end-of-line
;;   - C-b: backwards-char
;;   - C-f: forwards-char

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dependencies
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'functions)
(require 'clipboard)
(require 'screen-brightness)
(require 'pulse-audio)
(require 'scrot)
(require 'ivy-clipmenu)
(require 'general)
(require 'exwm)
(require 'vterm-mgt)
(require 'buffer)
(require 'device)
(require 'fonts)
(require 'bookmark)
(require 'tvl)
(require 'window-manager)
(require 'wpc-misc)

;; Note: The following lines must be sorted this way.
(setq evil-want-integration t)
(setq evil-want-keybinding nil)
(general-evil-setup)
(require 'evil)
(require 'evil-collection)
(require 'evil-commentary)
(require 'evil-surround)
(require 'key-chord)
(require 'edebug)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General Keybindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(general-mmap
  :keymaps 'override
  "RET" #'evil-goto-line
  "H"   #'evil-first-non-blank
  "L"   #'evil-end-of-line
  "_"   #'ranger
  "-"   #'dired-jump
  "sl"  #'functions-evil-window-vsplit-right
  "sh"  #'evil-window-vsplit
  "sk"  #'evil-window-split
  "sj"  #'functions-evil-window-split-down)

(general-nmap
  :keymaps 'override
  "gu" #'browse-url-at-point
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

(general-unbind 'motion "M-." "C-p" "<SPC>")
(general-unbind 'normal "s"   "M-." "C-p" "C-n")
(general-unbind 'insert "C-v" "C-d" "C-a" "C-e" "C-n" "C-p" "C-k")

(customize-set-variable 'evil-symbol-word-search t)
(evil-mode 1)
(evil-collection-init)
(evil-commentary-mode)
(global-evil-surround-mode 1)

;; Ensure the Evil search results get centered vertically.
;; When Emacs is run from a terminal, this forces Emacs to redraw itself, which
;; is visually disruptive.
(when window-system
  (progn
    (defadvice isearch-update
        (before advice-for-isearch-update activate)
      (evil-scroll-line-to-center (line-number-at-pos)))
    (defadvice evil-search-next
        (after advice-for-evil-search-next activate)
      (evil-scroll-line-to-center (line-number-at-pos)))
    (defadvice evil-search-previous
        (after advice-for-evil-search-previous activate)
      (evil-scroll-line-to-center (line-number-at-pos)))))

(key-chord-mode 1)
(key-chord-define evil-insert-state-map "jk" 'evil-normal-state)

;; This may be contraversial, but I never use the prefix key, and I'd prefer to
;; have to bound to the readline function that deletes the entire line.
(general-unbind "C-u")

(defmacro keybindings-exwm (c fn)
  "Bind C to FN using `exwm-input-set-key' with `kbd' applied to C."
  `(exwm-input-set-key (kbd ,c) ,fn))

(keybindings-exwm "C-M-v" #'ivy-clipmenu-copy)
(keybindings-exwm "<XF86MonBrightnessUp>" #'screen-brightness-increase)
(keybindings-exwm "<XF86MonBrightnessDown>" #'screen-brightness-decrease)
(keybindings-exwm "<XF86AudioMute>" #'pulse-audio-toggle-mute)
(keybindings-exwm "<XF86AudioLowerVolume>" #'pulse-audio-decrease-volume)
(keybindings-exwm "<XF86AudioRaiseVolume>" #'pulse-audio-increase-volume)
(keybindings-exwm "<XF86AudioMicMute>" #'pulse-audio-toggle-microphone)
(keybindings-exwm (kbd-raw 'x11 "s") #'scrot-select)
(keybindings-exwm "<C-M-tab>" #'window-manager-switch-to-exwm-buffer)
(keybindings-exwm (kbd-raw 'workspace "k") #'fonts-increase-size)
(keybindings-exwm (kbd-raw 'workspace "j") #'fonts-decrease-size)
(keybindings-exwm (kbd-raw 'workspace "0") #'fonts-reset-size)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Window sizing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(keybindings-exwm "C-M-=" #'balance-windows)
(keybindings-exwm "C-M-j" #'shrink-window)
(keybindings-exwm "C-M-k" #'enlarge-window)
(keybindings-exwm "C-M-h" #'shrink-window-horizontally)
(keybindings-exwm "C-M-l" #'enlarge-window-horizontally)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Window Management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(keybindings-exwm "M-h" #'windmove-left)
(keybindings-exwm "M-j" #'windmove-down)
(keybindings-exwm "M-k" #'windmove-up)
(keybindings-exwm "M-l" #'windmove-right)
(keybindings-exwm "M-\\" #'evil-window-vsplit)
(keybindings-exwm "M--" #'evil-window-split)
(keybindings-exwm "M-q" #'delete-window)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Miscellaneous
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(keybindings-exwm "M-:" #'eval-expression)
(keybindings-exwm "M-SPC" #'ivy-helpers-run-external-command)
(keybindings-exwm "M-x" #'counsel-M-x)
(keybindings-exwm "<M-tab>" #'window-manager-next-workspace)
(keybindings-exwm "<M-S-iso-lefttab>" #'window-manager-prev-workspace)
(keybindings-exwm "C-M-\\" #'ivy-pass)
(keybindings-exwm "s-e" #'wpc-misc-copy-emoji)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Workspaces
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(keybindings-exwm (kbd-raw 'workspace "l")
                  (lambda ()
                    (interactive)
                    (shell-command window-manager-screenlocker)))

(general-define-key
 :keymaps 'override
 "M-q" #'delete-window
 "<s-return>" #'toggle-frame-fullscreen
 "M-h" #'windmove-left
 "M-l" #'windmove-right
 "M-k" #'windmove-up
 "M-j" #'windmove-down
 "M-q" #'delete-window)

;; Support pasting in M-:.
(general-define-key
 :keymaps 'read-expression-map
 "C-v"   #'clipboard-yank
 "C-S-v" #'clipboard-yank)

(general-define-key
 :prefix "<SPC>"
 :states '(normal)
 "." #'ffap
 "gn" #'notmuch
 "i" #'counsel-semantic-or-imenu
 "I" #'ibuffer
 "hk" #'helpful-callable
 "hf" #'helpful-function
 "hm" #'helpful-macro
 "hc" #'helpful-command
 "hk" #'helpful-key
 "hv" #'helpful-variable
 "hp" #'helpful-at-point
 "hi" #'info-apropos
 "s" #'flyspell-mode
 "S" #'sort-lines
 "=" #'align
 "p" #'flycheck-previous-error
 "f" #'project-find-file
 "n" #'flycheck-next-error
 "N" #'smerge-next
 "W" #'balance-windows
 "gss" #'magit-status
 "gsd" #'tvl-depot-status
 "E" #'refine
 "es" #'functions-create-snippet
 "l" #'linum-mode
 "B" #'magit-blame
 "w" #'save-buffer
 "r" #'functions-evil-replace-under-point
 "R" #'deadgrep)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Vterm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Show or hide a vterm buffer.  I'm intentionally not defining this in
;; vterm-mgt.el because it consumes `buffer-show-previous', and I'd like to
;; avoid bloating vterm-mgt.el with dependencies that others may not want.
(general-define-key (kbd-raw 'x11 "t")
                    (lambda ()
                      (interactive)
                      (if (vterm-mgt--instance? (current-buffer))
                          (switch-to-buffer (first (buffer-source-code-buffers)))
                        (call-interactively #'vterm-mgt-find-or-create))))

(general-define-key
 :keymaps '(vterm-mode-map)
 ;; For some reason vterm captures this KBD instead of EXWM
 "C-S-f" nil
 "s-x" #'vterm-mgt-select
 "C-S-n" #'vterm-mgt-instantiate
 "C-S-w" #'vterm-mgt-kill
 "<C-tab>" #'vterm-mgt-next
 "<C-S-iso-lefttab>" #'vterm-mgt-prev
 "<s-backspace>" #'vterm-mgt-rename-buffer
 ;; Without this, typing "+" is effectively no-op. Try for yourself:
 ;; (vterm-send-key "<kp-add>")
 "<kp-add>" "+")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; notmuch
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; evil-collection adds many KBDs to notmuch modes. Some of these I find
;; disruptive.
(general-define-key
 :states '(normal)
 :keymaps '(notmuch-show-mode-map)
 "M-j" nil
 "M-k" nil
 "<C-S-iso-lefttab>" #'notmuch-show-previous-thread-show
 "<C-tab>" #'notmuch-show-next-thread-show
 "e" #'notmuch-show-archive-message-then-next-or-next-thread)

;; TODO(wpcarro): Consider moving this to a separate module
(defun keybindings--evil-ex-define-cmd-local (cmd f)
  "Define CMD to F locally to a buffer."
  (unless (local-variable-p 'evil-ex-commands)
    (setq-local evil-ex-commands (copy-alist evil-ex-commands)))
  (evil-ex-define-cmd cmd f))

;; TODO(wpcarro): Support a macro that can easily define evil-ex commands for a
;; particular mode.
;; Consumption:
;; (evil-ex-for-mode 'notmuch-message-mode
;;                   "x" #'notmuch-mua-send-and-exit)

(add-hook 'notmuch-message-mode-hook
          (lambda ()
            (keybindings--evil-ex-define-cmd-local "x" #'notmuch-mua-send-and-exit)))

;; For now, I'm mimmicking Gmail KBDs that I have memorized and enjoy
(general-define-key
 :states '(normal visual)
 :keymaps '(notmuch-search-mode-map)
 "M"  (lambda ()
        (interactive)
        (notmuch-search-tag '("-inbox" "+muted")))
 "mi" (lambda ()
        (interactive)
        (notmuch-search-tag '("+inbox" "-action" "-review" "-waiting" "-muted")))
 "ma" (lambda ()
        (interactive)
        (notmuch-search-tag '("-inbox" "+action" "-review" "-waiting")))
 "mr" (lambda ()
        (interactive)
        (notmuch-search-tag '("-inbox" "-action" "+review" "-waiting")))
 "mw" (lambda ()
        (interactive)
        (notmuch-search-tag '("-inbox" "-action" "-review" "+waiting")))
 "e" #'notmuch-search-archive-thread)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; magit
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(general-define-key
 :states '(normal)
 :keymaps '(magit-status-mode-map
            magit-log-mode-map
            magit-revision-mode-map)
 "l" #'evil-forward-char
 "L" #'magit-log)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Info-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; NOTE: I find some of the following, existing KBDs useful:
;;   M-x info-apropos
;;   u   Info-up
;;   M-n clone-buffer
(general-define-key
 :states '(normal)
 :keymaps '(Info-mode-map)
 "SPC" nil
 "g SPC" #'Info-scroll-up
 "RET" #'Info-follow-nearest-node
 "<C-tab>" #'Info-next
 "<C-S-iso-lefttab>" #'Info-prev
 "g l" #'Info-history-back
 "g t" #'Info-toc)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ibuffer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(general-define-key
 :states '(normal)
 :keymaps '(ibuffer-mode-map)
 "M-j" nil
 "K" #'ibuffer-do-delete)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; buffers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(general-define-key
 :states '(normal)
 "C-f" #'buffer-cycle-next
 "C-b" #'buffer-cycle-prev)

(general-define-key
 :prefix "<SPC>"
 :states '(normal)
 "b" #'buffer-ivy-source-code
 "<SPC>" #'buffer-show-previous
 "k" #'kill-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; edebug
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(general-define-key
 :states '(normal)
 :keymaps '(edebug-mode-map)
 ;; this restores my ability to move-left while debugging
 "h" nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; deadgrep
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(general-define-key
 :states '(normal)
 :keymaps '(deadgrep-mode-map)
 "<tab>" #'deadgrep-forward
 "<backtab>" #'deadgrep-backward)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; bookmarks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Install KBDs like <SPC>jb to search through my monorepo.
(bookmark-install-kbds)

(provide 'keybindings)
;;; keybindings.el ends here
