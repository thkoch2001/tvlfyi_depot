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
(require 'ring)
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
    ;; Yandex.Music -> `Я.Music<... stuff ...>'
    (`("Chromium-browser" ,(and (pred (lambda (title) (s-starts-with? "Yandex.Music - " title))) title))
     (format "Я.Music<%s>" (s-chop-prefix "Yandex.Music - " title)))

    ;; For other Chromium windows, make the title shorter.
    (`("Chromium-browser" ,title)
     (format "Chromium<%s>" (s-truncate 42 (s-chop-suffix " - Chromium" title))))

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

(let ((titlef (lambda ()
                (exwm-workspace-rename-buffer (create-window-name)))))
  (add-hook 'exwm-update-class-hook titlef)
  (add-hook 'exwm-update-title-hook titlef))

(fringe-mode 3)

;; tab-bar related config
(setq tab-bar-show 1)
(setq tab-bar-tab-hints t)
(setq tab-bar-new-tab-choice
      (lambda () (get-buffer-create "*scratch*")))

(tab-bar-mode 1)

(exwm-enable)

;; Tab-management shortcuts

(dotimes (i 8)
  (exwm-input-set-key (kbd (format "s-%d" i)) #'tab-bar-select-tab))
(exwm-input-set-key (kbd "s-9") #'tab-last)
(exwm-input-set-key (kbd "s-f") #'tab-next)
(exwm-input-set-key (kbd "s-b") #'tab-recent)
(exwm-input-set-key (kbd "s-w") #'tab-close)
(exwm-input-set-key (kbd "s-n") #'tab-new)

;; Launch applications / any command with completion (dmenu style!)
(exwm-input-set-key (kbd "s-d") #'run-xdg-app)
(exwm-input-set-key (kbd "s-x") #'run-external-command)
(exwm-input-set-key (kbd "s-p") #'password-store-lookup)

;; Add vterm selector to a key
(exwm-input-set-key (kbd "s-v") #'ts/switch-to-terminal)

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

;; Notmuch shortcuts as EXWM globals
;; (g m => gmail)
(exwm-input-set-key (kbd "s-g m") #'notmuch)

;; Let buffers move seamlessly between workspaces by making them
;; accessible in selectors on all frames.
(setq exwm-workspace-show-all-buffers t)
(setq exwm-layout-show-all-buffers t)

(provide 'desktop)
