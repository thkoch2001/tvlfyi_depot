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
;;
;; This used to use use-package, but when something breaks use-package
;; it doesn't exactly make debugging any easier.

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
(exwm-randr-enable)

;; Create 10 EXWM workspaces
(setq exwm-workspace-number 10)

;; 's-N': Switch to certain workspace, but switch back to the previous
;; one when tapping twice (emulates i3's `back_and_forth' feature)
(defvar *exwm-workspace-from-to* '(-1 . -1))
(defun exwm-workspace-switch-back-and-forth (target-idx)
  ;; If the current workspace is the one we last jumped to, and we are
  ;; asked to jump to it again, set the target back to the previous
  ;; one.
  (when (and (eq exwm-workspace-current-index (cdr *exwm-workspace-from-to*))
             (eq target-idx exwm-workspace-current-index))
    (setq target-idx (car *exwm-workspace-from-to*)))

  (setq *exwm-workspace-from-to*
        (cons exwm-workspace-current-index target-idx))

  (exwm-workspace-switch-create target-idx))

(dotimes (i 10)
  (exwm-input-set-key (kbd (format "s-%d" i))
                      `(lambda ()
                         (interactive)
                         (exwm-workspace-switch-back-and-forth ,i))))

;; Implement MRU functionality for EXWM workspaces, making it possible
;; to jump to the previous/next workspace very easily.
(defvar *recent-workspaces-ring* (make-ring 5)
  "Ring of recently used EXWM workspaces.")

(defvar *workspace-ring-is-rotating* nil
  "Variable used to track whether the workspace ring is rotating,
and suppress insertions into the ring in that case.")

(defun update-recent-workspaces ()
  "Hook run on EXWM workspace switches, adding new workspaces to the
ring."

  (unless *workspace-ring-is-rotating*
    (ring-remove+insert+extend *recent-workspaces-ring* exwm-workspace-current-index)))

(add-to-list 'exwm-workspace-switch-hook #'update-recent-workspaces)

(defun switch-to-previous-workspace ()
  "Switch to the previous workspace in the workspace ring."
  (interactive)

  (when-let ((*workspace-ring-is-rotating* t)
             (previous (condition-case err (ring-next *recent-workspaces-ring*
                                                      exwm-workspace-current-index)
                         ('error (message "No previous workspace in history!") nil))))
    (exwm-workspace-switch previous)))

(exwm-input-set-key (kbd "s-b") #'switch-to-previous-workspace)

(defun switch-to-next-workspace ()
  "Switch to the next workspace in the MRU workspace list."
  (interactive)

  (when-let ((*workspace-ring-is-rotating* t)
             (next (condition-case err (ring-previous *recent-workspaces-ring*
                                                      exwm-workspace-current-index)
                     ('error (message "No next workspace in history!") nil))))
    (exwm-workspace-switch next)))

(exwm-input-set-key (kbd "s-f") #'switch-to-next-workspace)

;; Provide a binding for jumping to a buffer on a workspace.
(defun exwm-jump-to-buffer ()
  "Jump to a workspace on which the target buffer is displayed."
  (interactive)
  (let ((exwm-layout-show-all-buffers nil)
        (initial exwm-workspace-current-index))
    (call-interactively #'exwm-workspace-switch-to-buffer)
    ;; After jumping, update the back-and-forth list like on a direct
    ;; index jump.
    (when (not (eq initial exwm-workspace-current-index))
      (setq *exwm-workspace-from-to*
            (cons initial exwm-workspace-current-index)))))

(exwm-input-set-key (kbd "C-c j") #'exwm-jump-to-buffer)

;; Tab-management shortcuts

(dotimes (i 8)
  (exwm-input-set-key (kbd (format "s-%d" (+ 1 i))) #'tab-bar-select-tab))

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
   '(("HDMI-A-0" 1 2 3 4 5 6 7 8)
     ("eDP" 9 0)))

  (shell-command "xrandr --output HDMI-A-0 --left-of eDP --auto")
  (exwm-randr-refresh))

(defun randr-tverskoy-tv ()
  "Split off a workspace to the TV over HDMI."
  (interactive)
  (set-randr-config
   '(("eDP" 1 2 3 4 5 6 7 8 9)
     ("HDMI-A-0" 0)))

  (shell-command "xrandr --output HDMI-A-0 --left-of eDP --mode 1920x1080")
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

(defun randr-khamovnik-layout-office ()
  "Use the left and right screen on khamovnik, in the office."
  (interactive)
  (set-randr-config `(("eDP-1" 1 2)
                      ("DP-2" 3 4 5 6 7 8 9 0)))

  (shell-command "xrandr --output DP-2 --mode 2560x1440 --primary --right-of eDP-1")
  (exwm-randr-refresh))

(defun randr-khamovnik-layout-home ()
  "Use the left and right screen on khamovnik, at home."
  (interactive)
  (set-randr-config `(("HDMI-1" 1 2 3 4 5 6 7 8)
                      ("eDP-1" 9 0)))

  (shell-command "xrandr --output HDMI-1 --auto --primary --left-of eDP-1")
  (exwm-randr-refresh))

(defun randr-khamovnik-layout-single ()
  "Use only the internal screen."
  (interactive)
  (set-randr-config '(("eDP-1" (number-sequence 0 9))))
  (shell-command "xrandr --output eDP-1 --auto --primary")
  (shell-command "xrandr --output DP-2 --off")
  (shell-command "xrandr --output HDMI-1 --off")
  (exwm-randr-refresh))

(pcase (s-trim (shell-command-to-string "hostname"))
  ("tverskoy"
   (exwm-input-set-key (kbd "s-m s") #'randr-tverskoy-layout-single)
   (exwm-input-set-key (kbd "s-m 2") #'randr-tverskoy-split-workspace))

  ("frog"
   (exwm-input-set-key (kbd "s-m b") #'randr-frog-layout-both)
   (exwm-input-set-key (kbd "s-m r") #'randr-frog-layout-right-only))

  ("khamovnik"
   (exwm-input-set-key (kbd "s-m 2") #'randr-khamovnik-layout-office)
   (exwm-input-set-key (kbd "s-m s") #'randr-khamovnik-layout-single)))

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

(exwm-randr-enable)

;; Let buffers move seamlessly between workspaces by making them
;; accessible in selectors on all frames.
(setq exwm-workspace-show-all-buffers t)
(setq exwm-layout-show-all-buffers t)

(provide 'desktop)
