;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dependencies
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'bookmark)
(require 'display)
(require 'window-manager)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(bookmark-install-kbd
 (make-bookmark :label "hadrian"
                :path "/hadrian"
                :kbd "h"))

(setq initial-buffer-choice "/hadrian")

(add-to-list 'ssh-hosts "wpcarro@tarasco")

(display-register primary
                  :output "DP-2"
                  :primary t
                  :coords (0 0)
                  :size (2560 1440)
                  :rate 30.0
                  :dpi 96
                  :rotate normal)

(display-register secondary
                  :output "DP-1"
                  :primary nil
                  :coords (2561 0)
                  :size (2560 1440)
                  :rate 30.0
                  :dpi 96
                  :rotate normal)

(display-arrangement main :displays (primary secondary))

(setq window-manager-named-workspaces
      (list (make-window-manager-named-workspace
             :label "Web Browsing"
             :kbd "c"
             :display display-secondary)
            (make-window-manager-named-workspace
             :label "Coding I"
             :kbd "1"
             :display display-primary)
            (make-window-manager-named-workspace
             :label "Coding II"
             :kbd "2"
             :display display-primary)
            (make-window-manager-named-workspace
             :label "Chatting"
             :kbd "h"
             :display display-secondary)))

;; I *think* this needs to be the last statement in this file.
(window-manager-init :init-hook #'display-arrange-main)
