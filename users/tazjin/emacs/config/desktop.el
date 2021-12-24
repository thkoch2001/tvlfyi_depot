;; -*- lexical-binding: t; -*-
;;
;; Configure desktop environment settings, including both
;; window-management (EXWM) as well as additional system-wide
;; commands.

(require 'dash)
(require 'exwm)
(require 'exwm-config)
(require 'exwm-randr)
(require 'exwm-systemtray)
(require 'exwm-xim )
(require 'f)
(require 's)

(defcustom tazjin--screen-lock-command "tazjin-screen-lock"
  "Command to execute for locking the screen."
  :group 'tazjin)

(defcustom tazjin--backlight-increase-command "light -A 4"
  "Command to increase screen brightness."
  :group 'tazjin)

(defcustom tazjin--backlight-decrease-command "light -U 4"
  "Command to decrease screen brightness."
  :group 'tazjin)

(defun pactl (cmd)
  (shell-command (concat "pactl " cmd))
  (message "Volume command: %s" cmd))

(defun volume-mute () (interactive) (pactl "set-sink-mute @DEFAULT_SINK@ toggle"))
(defun volume-up () (interactive) (pactl "set-sink-volume @DEFAULT_SINK@ +5%"))
(defun volume-down () (interactive) (pactl "set-sink-volume @DEFAULT_SINK@ -5%"))

(defun brightness-up ()
  (interactive)
  (shell-command tazjin--backlight-increase-command)
  (message "Brightness increased"))

(defun brightness-down ()
  (interactive)
  (shell-command tazjin--backlight-decrease-command)
  (message "Brightness decreased"))

(defun set-xkb-layout (layout)
  "Set the current X keyboard layout."

  (shell-command (format "setxkbmap %s" layout))
  (shell-command "setxkbmap -option caps:super")
  (message "Set X11 keyboard layout to '%s'" layout))

(defun lock-screen ()
  (interactive)
  (set-xkb-layout "us")
  (deactivate-input-method)
  (shell-command tazjin--screen-lock-command))

(defun create-window-name ()
  "Construct window names to be used for EXWM buffers by
  inspecting the window's X11 class and title.

  A lot of commonly used applications either create titles that
  are too long by default, or in the case of web
  applications (such as Cider) end up being constructed in
  awkward ways.

  To avoid this issue, some rewrite rules are applied for more
  human-accessible titles."

  (pcase (list (or exwm-class-name "unknown") (or exwm-title "unknown"))
    ;; In Cider windows, rename the class and keep the workspace/file
    ;; as the title.
    (`("Google-chrome" ,(and (pred (lambda (title) (s-ends-with? " - Cider" title))) title))
     (format "Cider<%s>" (s-chop-suffix " - Cider" title)))
    (`("Google-chrome" ,(and (pred (lambda (title) (s-ends-with? " - Cider V" title))) title))
     (format "Cider V<%s>" (s-chop-suffix " - Cider V" title)))

    ;; Attempt to detect IRCCloud windows via their title, which is a
    ;; combination of the channel name and network.
    ;;
    ;; This is what would often be referred to as a "hack". The regexp
    ;; will not work if a network connection buffer is selected in
    ;; IRCCloud, but since the title contains no other indication that
    ;; we're dealing with an IRCCloud window
    (`("Google-chrome"
       ,(and (pred (lambda (title)
                     (s-matches? "^[\*\+]\s#[a-zA-Z0-9/\-]+\s\|\s[a-zA-Z\.]+$" title)))
             title))
     (format "IRCCloud<%s>" title))

    ;; For other Chrome windows, make the title shorter.
    (`("Google-chrome" ,title)
     (format "Chrome<%s>" (s-truncate 42 (s-chop-suffix " - Google Chrome" title))))

    ;; Gnome-terminal -> Term
    (`("Gnome-terminal" ,title)
     ;; fish-shell buffers contain some unnecessary whitespace and
     ;; such before the current working directory. This can be
     ;; stripped since most of my terminals are fish shells anyways.
     (format "Term<%s>" (s-trim-left (s-chop-prefix "fish" title))))

    ;; Quassel buffers
    ;;
    ;; These have a title format that looks like:
    ;; "Quassel IRC - #tvl (hackint) — Quassel IRC"
    (`("quassel" ,title)
     (progn
       (if (string-match
            (rx "Quassel IRC - "
                (group (one-or-more (any alnum "[" "]" "&" "-" "#"))) ;; <-- channel name
                " (" (group (one-or-more (any ascii space))) ")" ;; <-- network name
                " — Quassel IRC")
            title)
           (format "Quassel<%s>" (match-string 2 title))
         title)))

    ;; For any other application, a name is constructed from the
    ;; window's class and name.
    (`(,class ,title) (format "%s<%s>" class (s-truncate 12 title)))))

;; EXWM launch configuration
;;
;; This used to use use-package, but when something breaks use-package
;; it doesn't exactly make debugging any easier.

(let ((titlef (lambda ()
                (exwm-workspace-rename-buffer (create-window-name)))))
  (add-hook 'exwm-update-class-hook titlef)
  (add-hook 'exwm-update-title-hook titlef))

(fringe-mode 3)
(exwm-enable)

;; 's-N': Switch to certain workspace
(setq exwm-workspace-number 10)
(dotimes (i 10)
  (exwm-input-set-key (kbd (format "s-%d" i))
                      `(lambda ()
                         (interactive)
                         (exwm-workspace-switch-create ,i))))

;; Launch applications / any command with completion (dmenu style!)
(exwm-input-set-key (kbd "s-d") #'counsel-linux-app)
(exwm-input-set-key (kbd "s-x") #'run-external-command)
(exwm-input-set-key (kbd "s-p") #'password-store-lookup)

;; Add X11 terminal selector to a key
(exwm-input-set-key (kbd "C-x t") #'ts/switch-to-terminal)

;; Toggle between line-mode / char-mode
(exwm-input-set-key (kbd "C-c C-t C-t") #'exwm-input-toggle-keyboard)

;; Volume keys
(exwm-input-set-key (kbd "<XF86AudioMute>") #'volume-mute)
(exwm-input-set-key (kbd "<XF86AudioRaiseVolume>") #'volume-up)
(exwm-input-set-key (kbd "<XF86AudioLowerVolume>") #'volume-down)

;; Brightness keys
(exwm-input-set-key (kbd "<XF86MonBrightnessDown>") #'brightness-down)
(exwm-input-set-key (kbd "<XF86MonBrightnessUp>") #'brightness-up)
(exwm-input-set-key (kbd "<XF86Display>") #'lock-screen)

;; Shortcuts for switching between keyboard layouts
(defmacro bind-xkb (lang key)
  `(exwm-input-set-key (kbd (format "s-%s" ,key))
                       (lambda ()
                         (interactive)
                         (set-xkb-layout ,lang))))

(bind-xkb "us" "k u")
(bind-xkb "de" "k d")
(bind-xkb "no" "k n")
(bind-xkb "ru" "k r")
(bind-xkb "se" "k s")

;; These are commented out because Emacs no longer starts (??) if
;; they're set at launch.
;;
(bind-xkb "us" "л г")
(bind-xkb "de" "л в")
(bind-xkb "no" "л т")
(bind-xkb "ru" "л к")

;; Configuration of EXWM input method handling for X applications
(exwm-xim-enable)
(setq default-input-method "russian-computer")
(push ?\C-\\ exwm-input-prefix-keys)

;; Line-editing shortcuts
(exwm-input-set-simulation-keys
 '(([?\C-d] . delete)
   ([?\C-w] . ?\C-c)))

;; Show time & battery status in the mode line
(display-time-mode)
(display-battery-mode)

;; enable display of X11 system tray within Emacs
(exwm-systemtray-enable)

;; Configure xrandr (multi-monitor setup).

(defun set-randr-config (screens)
  (setq exwm-randr-workspace-monitor-plist
        (-flatten (-map (lambda (screen)
                          (-map (lambda (screen-id) (list screen-id (car screen))) (cdr screen)))
                        screens))))

;; Layouts for Tverskoy (X13 AMD laptop)
(defun randr-tverskoy-layout-single ()
  "Laptop screen only!"
  (interactive)
  (set-randr-config '(("eDP" (number-sequence 0 9))))
  (shell-command "xrandr --output eDP --auto --primary")
  (shell-command "xrandr --output HDMI-A-0 --off")
  (exwm-randr-refresh))

(defun randr-tverskoy-split-workspace ()
  "Split the workspace across two screens, assuming external to the left."
  (interactive)
  (set-randr-config
   '(("HDMI-A-0" 1 2 3 4 5 6 7)
     ("eDP" 8 9 0)))

  (shell-command "xrandr --output HDMI-A-0 --left-of eDP --auto")
  (exwm-randr-refresh))

;; Layouts for frog (desktop)

(defun randr-frog-layout-right-only ()
  "Use only the right screen on frog."
  (interactive)
  (set-randr-config `(("DisplayPort-0" ,(number-sequence 0 9))))
  (shell-command "xrandr --output DisplayPort-0 --off")
  (shell-command "xrandr --output DisplayPort-1 --auto --primary"))

(defun randr-frog-layout-both ()
  "Use the left and right screen on frog."
  (interactive)
  (set-randr-config `(("DisplayPort-0" 1 2 3 4 5)
                      ("DisplayPort-1" 6 7 8 9 0)))

  (shell-command "xrandr --output DisplayPort-0 --auto --primary --left-of DisplayPort-1")
  (shell-command "xrandr --output DisplayPort-1 --auto --right-of DisplayPort-0 --rotate left"))

(pcase (s-trim (shell-command-to-string "hostname"))
  ("tverskoy"
   (exwm-input-set-key (kbd "s-m s") #'randr-tverskoy-layout-single)
   (exwm-input-set-key (kbd "s-m 2") #'randr-tverskoy-split-workspace))

  ("frog"
   (exwm-input-set-key (kbd "s-m b") #'randr-frog-layout-both)
   (exwm-input-set-key (kbd "s-m r") #'randr-frog-layout-right-only)))

;; Notmuch shortcuts as EXWM globals
;; (g m => gmail)
(exwm-input-set-key (kbd "s-g m") #'notmuch)
(exwm-input-set-key (kbd "s-g M") #'counsel-notmuch)

(exwm-randr-enable)

;; Let buffers move seamlessly between workspaces by making them
;; accessible in selectors on all frames.
(setq exwm-workspace-show-all-buffers t)
(setq exwm-layout-show-all-buffers t)

(provide 'desktop)
