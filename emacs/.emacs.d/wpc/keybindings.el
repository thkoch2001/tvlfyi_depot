;;; keybindings.el --- Centralizing my keybindings -*- lexical-binding: t -*-
;; Author: William Carroll <wpcarro@gmail.com>

;;; Commentary:
;; Attempting to centralize my keybindings to simplify my configuration.
;;
;; I have some expectations about my keybindings. Here are some of those
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

(require 'clipboard)
(require 'screen-brightness)
(require 'chrome)
(require 'scrot)
(require 'ivy-clipmenu)
(require 'general)
(require 'window-manager)
(require 'vterm-mgt)
(require 'buffer)
(require 'display)
(require 'device)
(require 'evil-ex)

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
  (general-unbind 'motion "M-." "C-p" "<SPC>")
  (general-unbind 'normal "s"   "M-." "C-p" "C-n")
  (general-unbind 'insert "C-v" "C-d" "C-a" "C-e" "C-n" "C-p" "C-k")
  (setq evil-symbol-word-search t)
  (evil-mode 1))

;; Ensure the Evil search results get centered vertically.
(progn
  (defadvice isearch-update
      (before advice-for-isearch-update activate)
    (evil-scroll-line-to-center (line-number-at-pos)))
  (defadvice evil-search-next
      (after advice-for-evil-search-next activate)
    (evil-scroll-line-to-center (line-number-at-pos)))
  (defadvice evil-search-previous
      (after advice-for-evil-search-previous activate)
    (evil-scroll-line-to-center (line-number-at-pos))))

(use-package evil-collection
  :after (evil)
  :config
  (evil-collection-init))

(use-package evil-magit)

;; create comments easily
(use-package evil-commentary
  :after (evil)
  :config
  (evil-commentary-mode))

(use-package evil-surround
  :after (evil)
  :config
  (global-evil-surround-mode 1))

(use-package key-chord
  :after (evil)
  :config
  (key-chord-mode 1)
  (key-chord-define evil-insert-state-map "jk" 'evil-normal-state))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General KBDs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This may be contraversial, but I never use the prefix key, and I'd prefer to
;; have to bound to the readline function that deletes the entire line.
(general-unbind "C-u")

(defmacro keybinding/exwm (c fn)
  "Bind C to FN using `exwm-input-set-key' with `kbd' applied to C."
  `(exwm-input-set-key (kbd ,c) ,fn))

(keybinding/exwm "C-M-v" #'ivy-clipmenu/copy)
(keybinding/exwm "<XF86MonBrightnessUp>" #'screen-brightness/increase)
(keybinding/exwm "<XF86MonBrightnessDown>" #'screen-brightness/decrease)
(keybinding/exwm "<XF86AudioMute>" #'pulse-audio/toggle-mute)
(keybinding/exwm "<XF86AudioLowerVolume>" #'pulse-audio/decrease-volume)
(keybinding/exwm "<XF86AudioRaiseVolume>" #'pulse-audio/increase-volume)
(keybinding/exwm "<XF86AudioMicMute>" #'pulse-audio/toggle-microphone)
(keybinding/exwm "C-M-c" #'chrome/browse)
(keybinding/exwm (kbd/raw 'x11 "s") #'scrot/select)
(keybinding/exwm "<C-M-tab>" #'exwm/switch-to-exwm-buffer)

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
 "s" #'flyspell-mode
 "S" #'sort-lines
 "=" #'align
 "p" #'flycheck-previous-error
 "f" #'project-find-file
 "n" #'flycheck-next-error
 "N" #'smerge-next
 "W" #'balance-windows
 "gs" #'magit-status
 "E" #'refine
 "es" #'wpc/create-snippet
 "l" #'linum-mode
 "B" #'magit-blame
 "w" #'save-buffer
 "r" #'wpc/evil-replace-under-point
 "R" #'deadgrep)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Vterm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Show or hide a vterm buffer.  I'm intentionally not defining this in
;; vterm-mgt.el because it consumes `buffer/show-previous', and I'd like to
;; avoid bloating vterm-mgt.el with dependencies that others may not want.
(general-define-key (kbd/raw 'x11 "t")
                    (lambda ()
                      (interactive)
                      (if (vterm-mgt--instance? (current-buffer))
                          (switch-to-buffer (first (buffer/source-code-buffers)))
                        (call-interactively #'vterm-mgt-find-or-create))))

(general-define-key
 :keymaps '(vterm-mode-map)
 "C-S-n" #'vterm-mgt-instantiate
 "C-S-w" #'vterm-mgt-kill
 "<C-tab>" #'vterm-mgt-next
 "<C-S-iso-lefttab>" #'vterm-mgt-prev
 "<s-backspace>" #'vterm-mgt-rename-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Displays
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (device/work-laptop?)
  (keybinding/exwm "<XF86Display>" #'display/cycle-display-states)
  (general-define-key
   :prefix "<SPC>"
   :states '(normal)
   "d0" #'display/disable-laptop
   "d1" #'display/enable-laptop
   "D0" #'display/disable-4k
   "D1" #'display/enable-4k))

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
(defun evil-ex-define-cmd-local (cmd f)
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
            (evil-ex-define-cmd-local "x" #'notmuch-mua-send-and-exit)))

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

(provide 'keybindings)
;;; keybindings.el ends here
