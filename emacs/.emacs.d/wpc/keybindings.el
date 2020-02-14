;;; keybindings.el --- Centralizing my keybindings -*- lexical-binding: t -*-
;; Author: William Carroll <wpcarro@gmail.com>

;;; Commentary:
;; Attempting to centralize my keybindings to simplify my configuration.

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
   "d1" #'display/enable-laptop)
  (general-define-key
   :prefix "<SPC>"
   :states '(normal)
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
 :states '(normal)
 :keymaps '(notmuch-search-mode-map)
 "e" #'notmuch-search-archive-thread)

(general-define-key
 :states '(normal)
 :prefix "<SPC>"
 "gn" #'notmuch)

(provide 'keybindings)
;;; keybindings.el ends here
