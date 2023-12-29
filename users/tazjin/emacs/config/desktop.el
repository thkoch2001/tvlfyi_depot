;; -*- lexical-binding: t; -*-
;;
;; Configure desktop environment settings, including both
;; window-management (EXWM) as well as additional system-wide
;; commands.

(require 'exwm)
(require 'exwm-config)
(require 'exwm-randr)
(require 'exwm-systemtray)
(require 'exwm-xim )
(require 'f)
(require 'ring)
(require 's)
(require 'seq)

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

(setq tab-bar-format
      '(tab-bar-format-history
        tab-bar-format-tabs tab-bar-separator
        tab-bar-format-align-right tab-bar-format-global))

(setq tab-bar-new-tab-choice
      (lambda () (get-buffer-create "*scratch*")))

(tab-bar-mode 1)

(setq x-no-window-manager t) ;; TODO(tazjin): figure out when to remove this
(exwm-enable)
(exwm-randr-enable)

;; Tab-management shortcuts

(defun tab-bar-select-or-return ()
  "This function behaves like `tab-bar-select-tab', except it calls
`tab-recent' if asked to jump to the current tab. This simulates
the back&forth behaviour of i3."
  (interactive)
  (let* ((key (event-basic-type last-command-event))
         (tab (if (and (characterp key) (>= key ?1) (<= key ?9))
                  (- key ?0)
                0))
         (current (1+ (tab-bar--current-tab-index))))
    (if (eq tab current)
        (tab-recent)
      (tab-bar-select-tab tab))))

(dotimes (i 8)
  (exwm-input-set-key (kbd (format "s-%d" (+ 1 i))) #'tab-bar-select-or-return))

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

;; Multi-monitor configuration.
;;
;; With tab-bar-mode, each monitor only displays at most one
;; workspace. Workspaces are only created, never deleted, meaning that
;; the number of workspaces will be equivalent to the maximum number
;; of displays that were connected during a session.
;;
;; The first workspace is special: It is kept on the primary monitor.

(defun exwm-assign-workspaces ()
  "Assigns workspaces to the currently existing monitors, putting
the first one on the primary display and allocating the others
dynamically if needed in no particular order."
  (interactive)
  (let* ((randr-monitors (exwm-randr--get-monitors))
         (primary (car randr-monitors))
         (all-monitors (seq-map #'car (cadr randr-monitors)))
         (sorted-primary-first (seq-sort (lambda (a b)
                                           (or (equal a primary)
                                               (< a b)))
                                         all-monitors))
         ;; assign workspace numbers to each monitor ...
         (workspace-assignments
          (flatten-list (seq-map-indexed (lambda (monitor idx)
                                           (list idx monitor))
                                         sorted-primary-first))))
    ;; ensure that the required workspaces exist
    (exwm-workspace-switch-create (- (seq-length all-monitors) 1))

    ;; update randr config
    (setq exwm-randr-workspace-monitor-plist workspace-assignments)
    (exwm-randr-refresh)

    ;; leave focus on primary workspace
    (exwm-workspace-switch 0)))

(defun list-available-monitors ()
  "List connected, but unused monitors."
  (let* ((all-connected
          (seq-map (lambda (line) (car (s-split " " line)))
                   (s-lines (s-trim (shell-command-to-string "xrandr | grep connected | grep -v disconnected")))))
         (all-active (seq-map #'car (cadr (exwm-randr--get-monitors)))))
    (seq-filter (lambda (s) (not (seq-contains-p all-active s)))
                all-connected)))

(defun exwm-enable-monitor ()
  "Interactively construct an EXWM invocation that enable the
given monitor and assigns a workspace to it."
  (interactive)

  (let* ((monitors (list-available-monitors))
         (primary (car (exwm-randr--get-monitors)))
         (monitor (pcase (seq-length monitors)
                    (0 (error "No available monitors."))
                    (1 (car monitors))
                    (_
                     (completing-read "Which monitor? " (list-available-monitors) nil t))))

         (configurations `(("secondary (left)" . ,(format "--left-of %s" primary))
                           ("secondary (right)" . ,(format "--right-of %s" primary))
                           ("primary (left)" . ,(format "--left-of %s --primary" primary))
                           ("primary (right)" . ,(format "--right-of %s --primary" primary))
                           ("mirror" . ,(format "--same-as %s" primary))))

         (where (completing-read (format "%s should be " monitor)
                                 (seq-map #'car configurations)
                                 nil t))
         (xrandr-pos (cdr (assoc where configurations)))
         (xrandr-cmd (format "xrandr --output %s --auto %s" monitor xrandr-pos)))
    (message "Invoking '%s'" xrandr-cmd)
    (shell-command xrandr-cmd)
    (exwm-assign-workspaces)))

(defun exwm-disable-monitor ()
  "Interactively choose a monitor to disable."
  (interactive)

  (let* ((all (exwm-randr--get-monitors))
         (active (seq-map #'car (cadr all)))
         (monitor (if (> (seq-length active) 1)
                      (completing-read "Disable which monitor? " active nil t)
                    (error "Only one monitor is active!")))

         ;; If this monitor was primary, pick another active one instead.
         (remaining (seq-filter (lambda (s) (not (equal s monitor))) active))
         (new-primary
          (when (equal monitor (car all))
            (pcase (seq-length remaining)
              (1 (car remaining))
              (_ (completing-read "New primary? " remaining nil t))))))

    (when new-primary
      (shell-command (format "xrandr --output %s --primary" new-primary)))

    (shell-command (format "xrandr --output %s --off" monitor))
    (exwm-assign-workspaces)))

(defun exwm-switch-monitor ()
  "Switch focus to another monitor by name."
  (interactive)

  ;; TODO: Filter out currently active? How to determine it?
  (let* ((target (completing-read "Switch to monitor: "
                                  (seq-map #'car (cadr (exwm-randr--get-monitors)))
                                  nil t))
         (target-workspace
          (cl-loop for (workspace screen) on exwm-randr-workspace-monitor-plist by #'cddr
                   when (equal screen target) return workspace)))
    (exwm-workspace-switch target-workspace)))

(exwm-input-set-key (kbd "s-m e") #'exwm-enable-monitor)
(exwm-input-set-key (kbd "s-m d") #'exwm-disable-monitor)
(exwm-input-set-key (kbd "s-m o") #'exwm-switch-monitor)

;; Notmuch shortcuts as EXWM globals
;; (g m => gmail)
(exwm-input-set-key (kbd "s-g m") #'notmuch)

;; Let buffers move seamlessly between workspaces by making them
;; accessible in selectors on all frames.
(setq exwm-workspace-show-all-buffers t)
(setq exwm-layout-show-all-buffers t)

(provide 'desktop)
