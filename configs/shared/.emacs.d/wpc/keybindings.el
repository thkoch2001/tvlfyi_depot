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

;; TODO: I need this because my Ergodox EZ sends super+shift instead of just
;; super. Remove this once I fix my Ergodox.
(keybinding/exwm "C-S-s-s" #'scrot/select)

(provide 'keybindings)
;;; keybindings.el ends here
