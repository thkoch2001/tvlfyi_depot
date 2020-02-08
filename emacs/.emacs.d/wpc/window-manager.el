;;; window-manager.el --- Functions augmenting my usage of EXWM. -*- lexical-binding: t -*-
;; Author: William Carroll <wpcarro@gmail.com>

;;; Commentary:
;; I switched to EXWM from i3, and I haven't looked back.  One day I may write a
;; poem declaring my love for Emacs and EXWM.  For now, I haven't the time.

;; Wish list:
;; - TODO: Support different startup commands and layouts depending on laptop or
;;   desktop.
;; - TODO: Support a Music named-workspace.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dependencies
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'alert)
(require 'prelude)
(require 'string)
(require 'cycle)
(require 'set)
(require 'kbd)
(require 'ivy-helpers)
(require 'display)
(require 'dotfiles)
(require 'org-helpers)
(require 'vterm-mgt)
(require 'dash)
(require 'evil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Library
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: Move this function to another module.
(defun pkill (name)
  "Call the pkill executable using NAME as its argument."
  (interactive "sProcess name: ")
  (call-process "pkill" nil nil nil name))

;; TODO: Associate `window-purpose' window-layouts with each of these named
;; workspaces.

;; TODO: Associate KBDs for each of these named-layouts.

;; TODO: Decide between window-manager, exwm, or some other namespace.

;; TODO: Support (cycle/from-list '(current previous)) to toggle back and forth
;; between most recent workspace.

;; TODO: Support ad hoc cycle for loading a few workspaces that can be cycled
;; between. (cycle/from-list '("Project" "Workspace"))

;; TODO: Consider supporting a workspace for Racket, Clojure, Common Lisp,
;; Haskell, Elixir, and a few other languages. These could behave very similarly
;; to repl.it, which I've wanted to have locally for awhile now.

;; TODO: Support MRU cache of workspaces for easily switching back-and-forth
;; between workspaces.

(cl-defstruct exwm/named-workspace
  label
  index
  kbd)

(defconst exwm/install-workspace-kbds? t
  "When t, install the keybindings to switch between named-workspaces.")

;; TODO: Consume `cache/touch' after changing workspaces.  Use this to enable
;; cycling through workspaces.

(defconst exwm/named-workspaces
  (list (make-exwm/named-workspace
         :label "Web surfing"
         :index 0
         :kbd "c")
        (make-exwm/named-workspace
         :label "Project"
         :index 1
         :kbd "p")
        (make-exwm/named-workspace
         :label "Briefcase"
         :index 2
         :kbd "d")
        (make-exwm/named-workspace
         :label "Scratch"
         :index 3
         :kbd "s")
        (make-exwm/named-workspace
         :label "Terminal"
         :index 4
         :kbd "t")
        (make-exwm/named-workspace
         :label "Todos"
         :index 5
         :kbd "o")
        (make-exwm/named-workspace
         :label "Chatter"
         :index 6
         :kbd "h")
        (make-exwm/named-workspace
         :label "IRC"
         :index 7
         :kbd "i")
        (make-exwm/named-workspace
         :label "Work"
         :index 8
         :kbd "w"))
  "List of `exwm/named-workspace' structs.")

;; Assert that no two workspaces share KBDs.
(prelude/assert (= (list/length exwm/named-workspaces)
                   (->> exwm/named-workspaces
                        (list/map #'exwm/named-workspace-kbd)
                        set/from-list
                        set/count)))

(defun window-manager/alert (x)
  "Message X with a structured format."
  (alert (string/concat "[exwm] " x)))

;; Use Emacs as my primary window manager.
(use-package exwm
  :config
  (require 'exwm-config)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Multiple Displays
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (require 'exwm-randr)
  (exwm-randr-enable)
  ;; TODO: Consider generating this plist.
  ;; TODO: Replace integer index values with their named workspace equivalents.
  (setq exwm-randr-workspace-monitor-plist
        (list 0 display/4k-monitor
              1 display/laptop-monitor))

  (evil-set-initial-state 'exwm-mode 'emacs)
  (ido-mode 1)
  (exwm-config-ido)
  (setq exwm-workspace-number
        (list/length exwm/named-workspaces))
  ;; EXWM supports "line-mode" and "char-mode".
  ;;
  ;; Note: It appears that calling `exwm-input-set-key' works if it's called
  ;; during startup.  Once a session has started, it seems like this function is
  ;; significantly less useful.  Is this a bug?
  ;;
  ;; Glossary:
  ;; - char-mode: All keystrokes except `exwm' global ones are passed to the
  ;;   application.
  ;; - line-mode:
  ;;
  ;; `exwm-input-global-keys' = {line,char}-mode; can also call `exwm-input-set-key'
  ;; `exwm-mode-map'          = line-mode
  ;; `???'                    = char-mode. Is there a mode-map for this?
  ;;
  ;; TODO: What is `exwm-input-prefix-keys'?
  ;; TODO: Once I get `exwm-input-global-keys' functions, drop support for
  ;; `wpc/kbds'.
  (let ((kbds `(
                ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                ;; Window sizing
                ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

                (:key "C-M-=" :fn balance-windows)
                ;; TODO: Make sure these don't interfere with LISP KBDs.
                (:key "C-M-j" :fn shrink-window)
                (:key "C-M-k" :fn enlarge-window)
                (:key "C-M-h" :fn shrink-window-horizontally)
                (:key "C-M-l" :fn enlarge-window-horizontally)

                ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                ;; Window traversing
                ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

                (:key "M-h" :fn windmove-left)
                (:key "M-j" :fn windmove-down)
                (:key "M-k" :fn windmove-up)
                (:key "M-l" :fn windmove-right)

                ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                ;; Window splitting
                ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

                (:key "M-\\" :fn evil-window-vsplit)
                (:key "M--"  :fn evil-window-split)

                ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                ;; Window deletion
                ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

                (:key "M-q" :fn delete-window)

                ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                ;; Miscellaneous
                ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

                (:key "M-:"               :fn eval-expression)
                (:key "M-SPC"             :fn window-manager/apps)
                (:key "M-x"               :fn counsel-M-x)
                (:key "<M-tab>"           :fn exwm/next-workspace)
                (:key "<M-S-iso-lefttab>" :fn exwm/prev-workspace)
                (:key "<M-iso-lefttab>"   :fn exwm/prev-workspace)
                (:key "C-M-\\"            :fn ivy-pass)

                ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                ;; Workspaces
                ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

                (:key ,(kbd/raw 'workspace "l") :fn window-manager/logout)
                (:key ,(kbd/raw 'workspace "i") :fn exwm/toggle-mode))))
    (setq exwm-input-global-keys
          (->> kbds
               (-map (lambda (plist)
                       `(,(kbd (plist-get plist :key)) . ,(plist-get plist :fn)))))))
  (setq exwm-input-simulation-keys
        ;; TODO: Consider supporting M-d and other readline style KBDs.
        '(([?\C-b] . [left])
          ([?\M-b] . [C-left])
          ([?\C-f] . [right])
          ([?\M-f] . [C-right])
          ([?\C-p] . [up])
          ([?\C-n] . [down])
          ([?\C-a] . [home])
          ([?\C-e] . [end])
          ([?\C-d] . [delete])
          ;; TODO: Assess whether or not this is a good idea.
          ;; TODO: Ensure C-c copies.
          ([?\C-c] . [C-c])))
  (exwm-enable))

;; TODO: Package workspace management in another module.

;; Here is the code required to allow EXWM to cycle workspaces.
(defconst exwm/workspaces
  (->> exwm/named-workspaces
       cycle/from-list)
  "Cycle of the my EXWM workspaces.")

(prelude/assert
 (= exwm-workspace-number
    (list/length exwm/named-workspaces)))

(defun exwm/next-workspace ()
  "Cycle forwards to the next workspace."
  (interactive)
  (exwm/change-workspace (cycle/next exwm/workspaces)))

(defun exwm/prev-workspace ()
  "Cycle backwards to the previous workspace."
  (interactive)
  (exwm/change-workspace (cycle/prev exwm/workspaces)))

;; TODO: Create friendlier API for working with EXWM.

;; Here is the code required to toggle EXWM's modes.
(defun exwm/line-mode ()
  "Switch exwm to line-mode."
  (call-interactively #'exwm-input-grab-keyboard)
  (window-manager/alert "Switched to line-mode"))

(defun exwm/char-mode ()
  "Switch exwm to char-mode."
  (call-interactively #'exwm-input-release-keyboard)
  (window-manager/alert "Switched to char-mode"))

(defconst exwm/modes
  (cycle/from-list (list #'exwm/char-mode
                         #'exwm/line-mode))
  "Functions to switch exwm modes.")

(defun exwm/toggle-mode ()
  "Switch between line- and char- mode."
  (interactive)
  (with-current-buffer (window-buffer)
    (when (eq major-mode 'exwm-mode)
      (funcall (cycle/next exwm/modes)))))

;; Ensure exwm apps open in char-mode.
(add-hook
 'exwm-manage-finish-hook
 #'exwm/char-mode)

;; Interface to the Linux password manager
;; TODO: Consider writing a better client for this.
(use-package ivy-pass)

;; TODO: Prefer a more idiomatic Emacs way like `with-output-to-temp-buffer'.

;; TODO: Create a mode similar to `help-mode' that also kills the buffer when
;; "q" is pressed since this is sensitive information that we probably don't
;; want persisting.

;; TODO: Have this interactively show all of the listings in ~/.password-store
;; in an ivy list.
(defun password-store/show (key)
  "Show the contents of KEY from the password-store in a buffer."
  (interactive)
  (let ((b (buffer/find-or-create (string/format "*password-store<%s>*" key))))
    (with-current-buffer b
      (insert (password-store-get key))
      (help-mode))
    (buffer/show b)))

;; TODO: How do I handle this dependency?
(defconst exwm/preferred-browser "google-chrome"
  "My preferred web browser.")

(defun exwm/browser-open (&optional url)
  "Opens the URL in `exwm/preferred-browser'."
  (exwm/open
   (string/format "%s %s" exwm/preferred-browser url)
   :buffer-name (string/format "*%s*<%s>" exwm/preferred-browser url)
   :process-name url))

;; TODO: Support searching all "launchable" applications like OSX's Spotlight.
;; TODO: Model this as key-value pairs.
(defconst window-manager/applications
  (list "google-chrome --new-window --app=https://chat.google.com"
        "google-chrome --new-window --app=https://calendar.google.com"
        "google-chrome --new-window --app=https://gmail.com"
        "telegram-desktop"
        "google-chrome --new-window --app=https://teknql.slack.com"
        "google-chrome --new-window --app=https://web.whatsapp.com"
        "google-chrome --new-window --app=https://irccloud.com"
        exwm/preferred-browser)
  "Applications that I commonly use.
These are the types of items that would usually appear in dmenu.")

;; TODO: Consider replacing the `ivy-read' call with something like `hydra' that
;; can provide a small mode for accepting user-input.
;; TODO: Put this somewhere more diliberate.

;; TODO: Configure the environment variables for xsecurelock so that the font is
;; smaller, different, and the glinux wallpaper doesn't show.
;; - XSECURELOCK_FONT="InputMono-Black 10"
;; - XSECURE_SAVER=""
;; - XSECURE_LOGO_IMAGE=""
;; Maybe just create a ~/.xsecurelockrc
;; TODO: Is there a shell-command API that accepts an alist and serializes it
;; into variables to pass to the shell command?
(defconst window-manager/xsecurelock
  "/usr/share/goobuntu-desktop-files/xsecurelock.sh"
  "Path to the proper xsecurelock executable.
The other path to xsecurelock is /usr/bin/xsecurelock, which works fine, but it
is not optimized for Goobuntu devices.  Goobuntu attempts to check a user's
password using the network.  When there is no network connection available, the
login attempts fail with an \"unknown error\", which isn't very helpful.  To
avoid this, prefer the goobuntu wrapper around xsecurelock when on a goobuntu
device.  This all relates to PAM (i.e. pluggable authentication modules).")

(defun window-manager/logout ()
  "Prompt the user for options for logging out, shutting down, etc.

The following options are supported:
- Lock
- Logout
- Suspend
- Hibernate
- Reboot
- Shutdown

Ivy is used to capture the user's input."
  (interactive)
  (let* ((name->cmd `(("Lock" . ,window-manager/xsecurelock)
                      ("Logout" . "sudo systemctl stop lightdm")
                      ("Suspend" . ,(string/concat
                                     window-manager/xsecurelock
                                     " && systemctl suspend"))
                      ("Hibernate" . ,(string/concat
                                       window-manager/xsecurelock
                                       " && systemctl hibernate"))
                      ("Reboot" . "systemctl reboot")
                      ("Shutdown" . "systemctl poweroff"))))
    (funcall
     (lambda ()
       (shell-command
        (alist/get (ivy-read "System: " (alist/keys name->cmd))
                   name->cmd))))))

(cl-defun exwm/open (command &key
                             (process-name command)
                             (buffer-name command))
  "Open COMMAND, which should be an X11 window."
  (start-process-shell-command process-name buffer-name command))

(cl-defun window-manager/execute-from-counsel (&key prompt list)
  "Display a counsel menu of `LIST' with `PROMPT' and pipe the output through
`start-process-shell-command'."
  (let ((x (ivy-read prompt list)))
    (exwm/open
     x
     :buffer-name (string/format "*exwm/open*<%s>" x)
     :process-name x)))

(defun window-manager/apps ()
  "Open commonly used applications from counsel."
  (interactive)
  (window-manager/execute-from-counsel
   :prompt "Application: "
   :list window-manager/applications))

(defun exwm/label->index (label workspaces)
  "Return the index of the workspace in WORKSPACES named LABEL."
  (let ((workspace (->> workspaces
                        (list/find
                         (lambda (x)
                           (equal label
                                  (exwm/named-workspace-label x)))))))
    (if (prelude/set? workspace)
        (exwm/named-workspace-index workspace)
      (error (string/concat "No workspace found for label: " label)))))

(defun exwm/register-kbd (workspace)
  "Registers a keybinding for WORKSPACE struct.
Currently using super- as the prefix for switching workspaces."
  (let ((handler (lambda ()
                   (interactive)
                   (exwm/switch (exwm/named-workspace-label workspace))))
        (key (exwm/named-workspace-kbd workspace)))
    (exwm-input-set-key
     (kbd/for 'workspace key)
     handler)))

(defun exwm/change-workspace (workspace)
  "Switch EXWM workspaces to the WORKSPACE struct."
  (exwm-workspace-switch (exwm/named-workspace-index workspace))
  (window-manager/alert
   (string/format "Switched to: %s" (exwm/named-workspace-label workspace))))

(defun exwm/switch (label)
  "Switch to a named workspaces using LABEL."
  (cycle/focus (lambda (x)
                 (equal label
                        (exwm/named-workspace-label x)))
               exwm/workspaces)
  (exwm/change-workspace (cycle/current exwm/workspaces)))

;; TODO: Assign an easy-to-remember keybinding to this.
(exwm-input-set-key (kbd "C-S-f") #'exwm/toggle-previous)
(defun exwm/toggle-previous ()
  "Focus the previously active EXWM workspace."
  (interactive)
  (exwm/change-workspace (cycle/focus-previous! exwm/workspaces)))

(defun exwm/switch-to-exwm-workspace ()
  "Use ivy to switched between named workspaces."
  (interactive)
  (ivy-read
   "Workspace: "
   (->> exwm/named-workspaces
        (list/map #'exwm/named-workspace-label))
   :action #'exwm/switch))

(defun exwm/exwm-buffer? (x)
  "Return t if buffer X is an EXWM buffer."
  (equal 'exwm-mode (buffer-local-value 'major-mode x)))

(defun exwm/application-name (buffer)
  "Return the name of the application running in the EXWM BUFFER.
This function asssumes that BUFFER passes the `exwm/exwm-buffer?' predicate."
  (with-current-buffer buffer exwm-class-name))

;; TODO: Support disambiguating between two or more instances of the same
;; application. For instance if two `exwm-class-name' values are
;; "Google-chrome", find a encode this information in the `buffer-alist'.
(defun exwm/switch-to-exwm-buffer ()
  "Use `completing-read' to focus an EXWM buffer."
  (interactive)
  (let* ((buffer-alist (->> (buffer-list)
                            (-filter #'exwm/exwm-buffer?)
                            (-map (lambda (buffer)
                                    (cons (exwm/application-name buffer)
                                          buffer)))))
         (label (completing-read "Switch to EXWM buffer: " buffer-alist)))
    (exwm-workspace-switch-to-buffer
     (alist-get label buffer-alist nil nil #'string=))))

(when exwm/install-workspace-kbds?
  (progn
    (->> exwm/named-workspaces
         (list/map #'exwm/register-kbd))
    (window-manager/alert "Registered workspace KBDs!")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Startup Applications in `exwm/named-workspaces'
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook
 'exwm-init-hook
 (lambda ()
   ;; TODO: Refactor this into a bigger solution where the named-workspaces are
   ;; coupled to their startup commands.  Expedience wins this time.
   (progn
     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;; Chrome
     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     (progn
       (exwm/switch "Web surfing")
       ;; make sure this blocks.
       ;; TODO: Support shell-cmd.el that has `shell-cmd/{sync,async}'.
       ;; (call-process-shell-command "google-chrome")
       )
     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;; Project
     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     (progn
       (exwm/switch "Project")
       (find-file constants/current-project))
     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;; Scratch
     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     (progn
       (exwm/switch "Scratch")
       (switch-to-buffer "*scratch*"))
     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;; Terminal
     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     (progn
       (exwm/switch "Terminal")
       (vterm-mgt-instantiate))
     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;; Todos
     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     (progn
       (exwm/switch "Todos")
       (org-helpers/find-file "today-expected.org")
       (wpc/evil-window-vsplit-right)
       (org-helpers/find-file "today-actual.org"))
     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;; Dotfiles
     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     (progn
       (exwm/switch "Briefcase")
       (dotfiles/find-emacs-file "init.el"))
     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;; Chatter
     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     (progn
       (exwm/switch "Chatter")
       ;; TODO: Support the following chat applications:
       ;; - Slack teknql
       ;; - irccloud.net
       ;; - web.whatsapp.com
       ;; - Telegram
       ;; NOTE: Perhaps all of these should be borderless.
       ;; (call-process-shell-command "terminator")
       )
     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;; Work
     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     (progn
       (exwm/switch "Work")
       ;; TODO: Support opening the following in chrome:
       ;; - calendar
       ;; - gmail
       ;; - chat (in a horizontal split)
       )
     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;; Reset to default
     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     (exwm/switch "Briefcase"))))

(provide 'window-manager)
;;; window-manager.el ends here
