;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dependencies
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'display)
(require 'window-manager)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Monitor Configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(display-register laptop
                  :output "eDP-1"
                  :primary t
                  :coords (0 0)
                  :size (1920 1080)
                  :rate 30.0
                  :dpi 96
                  :rotate normal)

(display-arrangement primary :displays (laptop))

(setq window-manager-named-workspaces
      (list (make-window-manager-named-workspace
             :label "Web Browsing"
             :kbd "c"
             :display display-laptop)
            (make-window-manager-named-workspace
             :label "Coding"
             :kbd "d"
             :display display-laptop)
            (make-window-manager-named-workspace
             :label "Chatting"
             :kbd "h"
             :display display-laptop)))

(window-manager-init :init-hook #'display-arrange-primary)
