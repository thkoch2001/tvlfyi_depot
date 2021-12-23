;;; window-manager.el --- Functions augmenting my usage of EXWM -*- lexical-binding: t -*-

;; Author: William Carroll <wpcarro@gmail.com>
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1"))

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
(require 'al)
(require 'prelude)
(require 'string)
(require 'cycle)
(require 'set)
(require 'kbd)
(require 'ivy-helpers)
(require 'display)
(require 'vterm-mgt)
(require 'dash)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Library
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: Associate `window-purpose' window-layouts with each of these named
;; workspaces.

;; TODO: Associate KBDs for each of these named-layouts.

;; TODO: Decide between window-manager, exwm, or some other namespace.

;; TODO: Support (cycle-from-list '(current previous)) to toggle back and forth
;; between most recent workspace.

;; TODO: Support ad hoc cycle for loading a few workspaces that can be cycled
;; between. (cycle-from-list '("Project" "Workspace"))

;; TODO: Consider supporting a workspace for Racket, Clojure, Common Lisp,
;; Haskell, Elixir, and a few other languages. These could behave very similarly
;; to repl.it, which I've wanted to have locally for awhile now.

;; TODO: Support MRU cache of workspaces for easily switching back-and-forth
;; between workspaces.

(cl-defstruct window-manager--named-workspace label kbd display)

(defconst window-manager--install-kbds? t
  "When t, install the keybindings to switch between named-workspaces.")

;; TODO: Consume `cache/touch' after changing workspaces.  Use this to enable
;; cycling through workspaces.

(defconst window-manager--named-workspaces
  (list (make-window-manager--named-workspace
         :label "Web Browsing"
         :kbd "c"
         :display display-4k-horizontal)
        (make-window-manager--named-workspace
         :label "Coding"
         :kbd "d"
         :display display-4k-horizontal)
        (make-window-manager--named-workspace
         :label "Vertical"
         :kbd "h"
         :display display-4k-vertical)
        (make-window-manager--named-workspace
         :label "Laptop"
         :kbd "p"
         :display display-laptop))
  "List of `window-manager--named-workspace' structs.")

;; Assert that no two workspaces share KBDs.
(prelude-assert (= (list-length window-manager--named-workspaces)
                   (->> window-manager--named-workspaces
                        (list-map #'window-manager--named-workspace-kbd)
                        set-from-list
                        set-count)))

(defun window-manager--alert (x)
  "Message X with a structured format."
  (alert (string-concat "[exwm] " x)))

;; Use Emacs as my primary window manager.
(use-package exwm
  :config
  (require 'exwm-config)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Multiple Displays
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (require 'exwm-randr)
  (exwm-randr-enable)
  (setq exwm-randr-workspace-monitor-plist
        (->> window-manager--named-workspaces
             (-map-indexed (lambda (i x)
                             (list i (window-manager--named-workspace-display x))))
             -flatten))
  (setq exwm-workspace-number (list-length window-manager--named-workspaces))
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

;; Here is the code required to allow EXWM to cycle workspaces.
(defconst window-manager--workspaces
  (->> window-manager--named-workspaces
       cycle-from-list)
  "Cycle of the my EXWM workspaces.")

(prelude-assert
 (= exwm-workspace-number
    (list-length window-manager--named-workspaces)))

(defun window-manager-next-workspace ()
  "Cycle forwards to the next workspace."
  (interactive)
  (window-manager--change-workspace (cycle-next window-manager--workspaces)))

(defun window-manager-prev-workspace ()
  "Cycle backwards to the previous workspace."
  (interactive)
  (window-manager--change-workspace (cycle-prev window-manager--workspaces)))

;; TODO: Create friendlier API for working with EXWM.

;; Here is the code required to toggle EXWM's modes.
(defun window-manager--line-mode ()
  "Switch exwm to line-mode."
  (call-interactively #'exwm-input-grab-keyboard)
  (window-manager--alert "Switched to line-mode"))

(defun window-manager--char-mode ()
  "Switch exwm to char-mode."
  (call-interactively #'exwm-input-release-keyboard)
  (window-manager--alert "Switched to char-mode"))

(defconst window-manager--modes
  (cycle-from-list (list #'window-manager--char-mode
                         #'window-manager--line-mode))
  "Functions to switch exwm modes.")

(defun window-manager-toggle-mode ()
  "Switch between line- and char- mode."
  (interactive)
  (with-current-buffer (window-buffer)
    (when (eq major-mode 'exwm-mode)
      (funcall (cycle-next window-manager--modes)))))

;; Ensure exwm apps open in char-mode.
(add-hook 'exwm-manage-finish-hook #'window-manager--char-mode)

;; Interface to the Linux password manager
;; TODO: Consider writing a better client for this.
(use-package ivy-pass)

;; TODO: How do I handle this dependency?
(defconst window-manager--preferred-browser "google-chrome"
  "My preferred web browser.")

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
(defconst window-manager--xsecurelock
  (if (device-corporate?)
      "/usr/share/goobuntu-desktop-files/xsecurelock.sh"
    "xsecurelock")
  "Path to the proper xsecurelock executable.
The other path to xsecurelock is /usr/bin/xsecurelock, which works fine, but it
is not optimized for Goobuntu devices.  Goobuntu attempts to check a user's
password using the network.  When there is no network connection available, the
login attempts fail with an \"unknown error\", which isn't very helpful.  To
avoid this, prefer the goobuntu wrapper around xsecurelock when on a goobuntu
device.  This all relates to PAM (i.e. pluggable authentication modules).")

(defun window-manager-logout ()
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
  (let* ((name->cmd `(("Lock" .
                       (lambda ()
                         (shell-command window-manager--xsecurelock)))
                      ("Logout" .
                       (lambda ()
                         (let ((default-directory "/sudo::"))
                           (shell-command "systemctl stop lightdm"))))
                      ("Suspend" .
                       (lambda ()
                         (shell-command "systemctl suspend")))
                      ("Hibernate" .
                       (lambda ()
                         (shell-command "systemctl hibernate")))
                      ("Reboot" .
                       (lambda ()
                         (let ((default-directory "/sudo::"))
                           (shell-command "reboot"))))
                      ("Shutdown" .
                       (lambda ()
                         (let ((default-directory "/sudo::"))
                           (shell-command "shutdown now")))))))
    (funcall
     (lambda ()
       (funcall (al-get (ivy-read "System: " (al-keys name->cmd))
                        name->cmd))))))

(defun window-manager--label->index (label workspaces)
  "Return the index of the workspace in WORKSPACES named LABEL."
  (let ((index (-elem-index label (-map #'window-manager--named-workspace-label
                                        workspaces))))
    (if index index (error (format "No workspace found for label: %s" label)))))

(defun window-manager--register-kbd (workspace)
  "Registers a keybinding for WORKSPACE struct.
Currently using super- as the prefix for switching workspaces."
  (let ((handler (lambda ()
                   (interactive)
                   (window-manager--switch
                    (window-manager--named-workspace-label workspace))))
        (key (window-manager--named-workspace-kbd workspace)))
    (exwm-input-set-key
     (kbd-for 'workspace key)
     handler)))

(defun window-manager--change-workspace (workspace)
  "Switch EXWM workspaces to the WORKSPACE struct."
  (exwm-workspace-switch
   (window-manager--label->index
    (window-manager--named-workspace-label workspace)
    window-manager--named-workspaces))
  (window-manager--alert
   (string-format "Switched to: %s"
                  (window-manager--named-workspace-label workspace))))

(defun window-manager--switch (label)
  "Switch to a named workspaces using LABEL."
  (cycle-focus (lambda (x)
                 (equal label
                        (window-manager--named-workspace-label x)))
               window-manager--workspaces)
  (window-manager--change-workspace (cycle-current window-manager--workspaces)))

(exwm-input-set-key (kbd "C-S-f") #'window-manager-toggle-previous)

(defun window-manager-toggle-previous ()
  "Focus the previously active EXWM workspace."
  (interactive)
  (window-manager--change-workspace
   (cycle-focus-previous! window-manager--workspaces)))

(defun window-manager--exwm-buffer? (x)
  "Return t if buffer X is an EXWM buffer."
  (equal 'exwm-mode (buffer-local-value 'major-mode x)))

(defun window-manager--application-name (buffer)
  "Return the name of the application running in the EXWM BUFFER.
This function asssumes that BUFFER passes the `window-manager--exwm-buffer?'
predicate."
  (with-current-buffer buffer exwm-class-name))

;; TODO: Support disambiguating between two or more instances of the same
;; application. For instance if two `exwm-class-name' values are
;; "Google-chrome", find a encode this information in the `buffer-alist'.
(defun window-manager-switch-to-exwm-buffer ()
  "Use `completing-read' to focus an EXWM buffer."
  (interactive)
  (let* ((buffer-alist (->> (buffer-list)
                            (-filter #'window-manager--exwm-buffer?)
                            (-map
                             (lambda (buffer)
                               (cons (window-manager--application-name buffer)
                                     buffer)))))
         (label (completing-read "Switch to EXWM buffer: " buffer-alist)))
    (exwm-workspace-switch-to-buffer
     (al-get label buffer-alist))))

(when window-manager--install-kbds?
  (progn
    (->> window-manager--named-workspaces
         (list-map #'window-manager--register-kbd))
    (window-manager--alert "Registered workspace KBDs!")))

(defun window-manager-current-workspace ()
  "Output the label of the currently active workspace."
  (->> window-manager--workspaces
       cycle-current
       window-manager--named-workspace-label))

(defun window-manager-swap-workspaces ()
  "Prompt the user to switch the current workspace with another."
  (interactive)
  (let* ((selection (->> window-manager--named-workspaces
                         (-map #'window-manager--named-workspace-label)
                         (-reject
                          (lambda (x)
                            (s-equals? x (window-manager-current-workspace))))
                         (completing-read
                          (format "Swap current workspace (i.e. \"%s\") with: "
                                  (window-manager-current-workspace)))))
         (i (-find-index (lambda (x)
                           (s-equals? selection (window-manager--named-workspace-label x)))
                                 window-manager--named-workspaces)))
    (exwm-workspace-swap exwm-workspace--current (elt exwm-workspace--list i))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Startup Applications in `window-manager--named-workspaces'
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'exwm-init-hook
          (lambda ()
            ;; (display-arrange-primary)
            (window-manager--switch "Coding")))

(provide 'window-manager)
;;; window-manager.el ends here
