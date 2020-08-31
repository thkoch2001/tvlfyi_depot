;;; window-manager.el --- Functions augmenting my usage of EXWM -*- lexical-binding: t -*-

;; Author: William Carroll <wpcarro@gmail.com>
;; Version: 0.0.1
;; URL: https://git.wpcarro.dev/wpcarro/briefcase
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
(require 'prelude)
(require 'string)
(require 'cycle)
(require 'set)
(require 'kbd)
(require 'ivy-helpers)
(require 'display)
(require 'vterm-mgt)
(require 'dash)
(require 'evil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Library
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(cl-defstruct window-manager--named-workspace label kbd)

(defconst window-manager--install-kbds? t
  "When t, install the keybindings to switch between named-workspaces.")

;; TODO: Consume `cache/touch' after changing workspaces.  Use this to enable
;; cycling through workspaces.

(defconst window-manager--named-workspaces
  (list (make-window-manager--named-workspace
         :label "Web surfing"
         :kbd "c")
        (make-window-manager--named-workspace
         :label "Briefcase"
         :kbd "d")
        (make-window-manager--named-workspace
         :label "Todos"
         :kbd "o")
        (make-window-manager--named-workspace
         :label "Chatter"
         :kbd "h"))
  "List of `window-manager--named-workspace' structs.")

;; Assert that no two workspaces share KBDs.
(prelude-assert (= (list/length window-manager--named-workspaces)
                   (->> window-manager--named-workspaces
                        (list/map #'window-manager--named-workspace-kbd)
                        set/from-list
                        set/count)))

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
        (list 0 display/4k-monitor
              1 display/laptop-monitor))

  (evil-set-initial-state 'exwm-mode 'emacs)
  (setq exwm-workspace-number
        (list/length window-manager--named-workspaces))
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
                (:key "M-SPC"             :fn ivy-helpers/run-external-command)
                (:key "M-x"               :fn counsel-M-x)
                (:key "<M-tab>"           :fn window-manager-next-workspace)
                (:key "<M-S-iso-lefttab>" :fn window-manager-prev-workspace)
                (:key "<M-iso-lefttab>"   :fn window-manager-prev-workspace)
                (:key "C-M-\\"            :fn ivy-pass)

                ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                ;; Workspaces
                ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

                (:key ,(kbd/raw 'workspace "l") :fn window-manager-logout))))
    (setq exwm-input-global-keys
          (->> kbds
               (-map (lambda (plist)
                       `(,(kbd (plist-get plist :key)) .
                         ,(plist-get plist :fn)))))))
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
(defconst window-manager--workspaces
  (->> window-manager--named-workspaces
       cycle/from-list)
  "Cycle of the my EXWM workspaces.")

(prelude-assert
 (= exwm-workspace-number
    (list/length window-manager--named-workspaces)))

(defun window-manager-next-workspace ()
  "Cycle forwards to the next workspace."
  (interactive)
  (window-manager--change-workspace (cycle/next window-manager--workspaces)))

(defun window-manager-prev-workspace ()
  "Cycle backwards to the previous workspace."
  (interactive)
  (window-manager--change-workspace (cycle/prev window-manager--workspaces)))

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
  (cycle/from-list (list #'window-manager--char-mode
                         #'window-manager--line-mode))
  "Functions to switch exwm modes.")

(defun window-manager-toggle-mode ()
  "Switch between line- and char- mode."
  (interactive)
  (with-current-buffer (window-buffer)
    (when (eq major-mode 'exwm-mode)
      (funcall (cycle/next window-manager--modes)))))

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
  "/usr/share/goobuntu-desktop-files/xsecurelock.sh"
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
  (let* ((name->cmd `(("Lock" . ,window-manager--xsecurelock)
                      ("Logout" . "sudo systemctl stop lightdm")
                      ("Suspend" . ,(string-concat
                                     window-manager--xsecurelock
                                     " && systemctl suspend"))
                      ("Hibernate" . ,(string-concat
                                       window-manager--xsecurelock
                                       " && systemctl hibernate"))
                      ("Reboot" . "systemctl reboot")
                      ("Shutdown" . "systemctl poweroff"))))
    (funcall
     (lambda ()
       (shell-command
        (alist/get (ivy-read "System: " (alist/keys name->cmd))
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
     (kbd/for 'workspace key)
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
  (cycle/focus (lambda (x)
                 (equal label
                        (window-manager--named-workspace-label x)))
               window-manager--workspaces)
  (window-manager--change-workspace (cycle/current window-manager--workspaces)))

(exwm-input-set-key (kbd "C-S-f") #'window-manager-toggle-previous)

(defun window-manager-toggle-previous ()
  "Focus the previously active EXWM workspace."
  (interactive)
  (window-manager--change-workspace
   (cycle/focus-previous! window-manager--workspaces)))

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
     (alist-get label buffer-alist nil nil #'string=))))

(when window-manager--install-kbds?
  (progn
    (->> window-manager--named-workspaces
         (list/map #'window-manager--register-kbd))
    (window-manager--alert "Registered workspace KBDs!")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Startup Applications in `window-manager--named-workspaces'
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'exwm-init-hook (lambda () (window-manager--switch "Briefcase")))

(provide 'window-manager)
;;; window-manager.el ends here
