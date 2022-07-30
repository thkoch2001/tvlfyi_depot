;;; window-manager.el --- Functions augmenting my usage of EXWM -*- lexical-binding: t -*-

;; Author: William Carroll <wpcarro@gmail.com>
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1"))

;;; Commentary:
;; I switched to EXWM from i3, and I haven't looked back.  One day I may write a
;; poem declaring my love for Emacs and EXWM.  For now, I haven't the time.

;; Wist List:
;; - TODO: Consider supporting MRU cache of worksapces.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dependencies
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'alert)
(require 'cycle)
(require 'dash)
(require 'kbd)
(require 's)
(require 'exwm)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgroup window-manager nil
  "Customization options for `window-manager'.")

(cl-defstruct window-manager-named-workspace
  label kbd display)

(defcustom window-manager-named-workspaces nil
  "List of `window-manager-named-workspace' structs."
  :group 'window-manager
  :type (list 'window-manager-named-workspace))

(defcustom window-manager-screenlocker "xsecurelock"
  "Reference to a screen-locking executable."
  :group 'window-manager
  :type 'string)

(defvar window-manager--workspaces nil
  "Cycle of the my EXWM workspaces.")

(defconst window-manager--modes
  (cycle-from-list (list #'window-manager--char-mode
                         #'window-manager--line-mode))
  "Functions to switch exwm modes.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Library
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun window-manager--alert (x)
  "Message X with a structured format."
  (alert (s-concat "[exwm] " x)))

(cl-defun window-manager-init (&key init-hook)
  "Call `exwm-enable' alongside other bootstrapping functions."
  (require 'exwm-config)
  (require 'exwm-randr)
  (setq exwm-randr-workspace-monitor-plist
        (->> window-manager-named-workspaces
             (-map-indexed (lambda (i x)
                             (list i (window-manager-named-workspace-display x))))
             -flatten))
  (setq exwm-workspace-number (length window-manager-named-workspaces))
  (setq exwm-input-simulation-keys
        '(([?\C-b] . [left])
          ([?\M-b] . [C-left])
          ([?\C-f] . [right])
          ([?\M-f] . [C-right])
          ([?\C-p] . [up])
          ([?\C-n] . [down])
          ([?\C-a] . [home])
          ([?\C-e] . [end])
          ([?\C-d] . [delete])
          ([?\C-c] . [C-c])))
  ;; Install workspace KBDs
  (progn
    (->> window-manager-named-workspaces
         (list-map #'window-manager--register-kbd))
    (window-manager--alert "Registered workspace KBDs!"))
  ;; Ensure exwm apps open in char-mode.
  (add-hook 'exwm-manage-finish-hook #'window-manager--char-mode)
  (add-hook 'exwm-init-hook init-hook)
  (setq window-manager--workspaces
        (cycle-from-list window-manager-named-workspaces))
  (exwm-randr-enable)
  (exwm-enable))

(defun window-manager-next-workspace ()
  "Cycle forwards to the next workspace."
  (interactive)
  (window-manager--change-workspace (cycle-next! window-manager--workspaces)))

(defun window-manager-prev-workspace ()
  "Cycle backwards to the previous workspace."
  (interactive)
  (window-manager--change-workspace (cycle-prev! window-manager--workspaces)))

;; Here is the code required to toggle EXWM's modes.
(defun window-manager--line-mode ()
  "Switch exwm to line-mode."
  (call-interactively #'exwm-input-grab-keyboard)
  (window-manager--alert "Switched to line-mode"))

(defun window-manager--char-mode ()
  "Switch exwm to char-mode."
  (call-interactively #'exwm-input-release-keyboard)
  (window-manager--alert "Switched to char-mode"))

(defun window-manager-toggle-mode ()
  "Switch between line- and char- mode."
  (interactive)
  (with-current-buffer (window-buffer)
    (when (eq major-mode 'exwm-mode)
      (funcall (cycle-next! window-manager--modes)))))

(defun window-manager--label->index (label workspaces)
  "Return the index of the workspace in WORKSPACES named LABEL."
  (let ((index (-elem-index label (-map #'window-manager-named-workspace-label
                                        workspaces))))
    (if index index (error (format "No workspace found for label: %s" label)))))

(defun window-manager--register-kbd (workspace)
  "Registers a keybinding for WORKSPACE struct.
Currently using super- as the prefix for switching workspaces."
  (let ((handler (lambda ()
                   (interactive)
                   (window-manager--switch
                    (window-manager-named-workspace-label workspace))))
        (key (window-manager-named-workspace-kbd workspace)))
    (exwm-input-set-key
     (kbd-for 'workspace key)
     handler)))

(defun window-manager--change-workspace (workspace)
  "Switch EXWM workspaces to the WORKSPACE struct."
  (exwm-workspace-switch
   (window-manager--label->index
    (window-manager-named-workspace-label workspace)
    window-manager-named-workspaces))
  (window-manager--alert
   (format "Switched to: %s"
           (window-manager-named-workspace-label workspace))))

(defun window-manager--switch (label)
  "Switch to a named workspaces using LABEL."
  (cycle-focus! (lambda (x)
                  (equal label
                         (window-manager-named-workspace-label x)))
                window-manager--workspaces)
  (window-manager--change-workspace (cycle-current window-manager--workspaces)))

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

(defun window-manager-current-workspace ()
  "Output the label of the currently active workspace."
  (->> window-manager--workspaces
       cycle-current
       window-manager-named-workspace-label))

(defun window-manager-workspace-move ()
  "Prompt the user to move the current workspace to another."
  (interactive)
  (exwm-workspace-move
   exwm-workspace--current
   (window-manager--label->index
    (completing-read "Move current workspace to: "
                     (->> window-manager-named-workspaces
                          (-map #'window-manager-named-workspace-label))
                     nil
                     t)
    window-manager-named-workspaces)))

(defun window-manager-move-window ()
  "Prompt the user to move the current window to another workspace."
  (interactive)
  (let ((window (get-buffer-window))
        (dest (completing-read "Move current window to: "
                               (->> window-manager-named-workspaces
                                    (-map #'window-manager-named-workspace-label))
                               nil
                               t)))
    (exwm-workspace-move-window
     (exwm-workspace--workspace-from-frame-or-index
      (window-manager--label->index dest window-manager-named-workspaces))
     (exwm--buffer->id window))
    (window-manager--switch dest)))

(provide 'window-manager)
;;; window-manager.el ends here
