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

(provide 'keybindings)
;;; keybindings.el ends here
