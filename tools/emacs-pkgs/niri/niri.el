;;; niri.el --- seamless niri/emacs integration. -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 The TVL Contributors
;;
;; Author: Vincent Ambo <tazjin@tvl.su>
;; Version: 1.0
;; Package-Requires: ((emacs "27.1"))
;;
;;; Commentary:
;;
;; After having used EXWM for many years (7 or so?) it's become second nature
;; that there is no difference between windows and Emacs buffers. This means
;; that from any Emacs buffer (or, in the case of EXWM, from any X window) it's
;; possible to switch to any of the others.
;;
;; This implements similar logic for Emacs running in Niri, consisting of two
;; sides of the integration:
;;
;; # In Emacs
;;
;; Inside of Emacs, when switching buffers, populate the buffer-switching menu
;; additionally with all open Niri windows. Selecting a Niri window moves the
;; screen to that window.
;;
;; # Outside of Emacs
;;
;; Provides an interface for the same core functionality that can be used from
;; shell scripts, and bound to selectors like dmenu or rofi.
;;
;; # Switching to Emacs buffers
;;
;; Some special logic exists for handling the case of switching to an Emacs
;; buffer. There are several conditions that we can be in, that each have a
;; predictable result:
;;
;; In a non-Emacs window, selecting an Emacs buffer will either switch to an
;; Emacs frame already displaying this buffer, or launch a new frame for it.
;;
;; Inside of Emacs, if *another* frame is already displaying the buffer, switch
;; to it. Otherwise the behaviour is the same as standard buffer switching.

(require 'seq)
(require 'map)

(defun niri-list-windows ()
  "List all currently open Niri windows."
  (json-parse-string
   (shell-command-to-string "niri msg -j windows")
   :false-object nil))

(defun niri--window-is-emacs (window)
  (equal (map-elt window "app_id") "emacs"))

(defun niri--list-selectables ()
  "Lists all currently selectable things in a format that can work
with completing-read. Selectable means all open Niri
windows (except Emacs windows) and all Emacs buffers.

Emacs windows are returned separately, as they are required for
frame navigation."
  (let* (;; all niri windows, with emacs/non-emacs windows split up
         (all-windows (niri-list-windows))
         (windows (seq-filter (lambda (w) (not (niri--window-is-emacs w)))
                              all-windows))
         (emacs-windows (seq-filter #'niri--window-is-emacs all-windows))

         ;; all non-hidden buffers
         (buffers (seq-filter (lambda (b) (not (string-prefix-p " " (buffer-name b))))
                              (buffer-list)))
         (selectables (make-hash-table :test 'equal :size (+ (length windows)
                                                             (length buffers)))))
    (seq-do (lambda (window)
              (map-put! selectables (map-elt window "title")
                        (cons :niri window)))
            windows)

    (seq-do (lambda (buf)
              (map-put! selectables (buffer-name buf)
                        (cons :emacs buf)))
            buffers)
    (cons selectables emacs-windows)))

(defun niri--focus-window (window)
  (shell-command (format "niri msg action focus-window --id %d"
                         (map-elt window "id"))))

(defun niri--target-action-internal (target)
  "Focus the given TARGET (a Niri window or Emacs buffer). This is
used when called from inside of Emacs. It will NOT correctly
switch Niri windows when called from outside of Emacs."
  (pcase (car target)
    (:emacs (pop-to-buffer (cdr target) '((display-buffer-reuse-window
                                           display-buffer-same-window)
                                          (reusable-frames . 0))))
    (:niri (niri--focus-window (cdr target)))))

(defun niri-go-anywhere ()
  "Interactively select and switch to an open Niri window, or an
  Emacs buffer."
  (interactive)
  (let* ((selectables (car (niri--list-selectables)))
         ;; Annotate buffers that display remote files. I frequently
         ;; want to see it, because I might have identically named
         ;; files open locally and remotely at the same time, and it
         ;; helps with differentiating them.
         (completion-extra-properties
          '(:annotation-function
            (lambda (name)
              (let ((elt (map-elt selectables name)))
                (pcase (car elt)
                  (:emacs
                   (if-let* ((file (buffer-file-name (cdr elt)))
                             (remote (file-remote-p file)))
                       (format " [%s]" remote)))
                  (:niri (format " [%s]" (map-elt (cdr elt) "app_id"))))))))

         (target-key (completing-read "Switch to: " (map-keys selectables)))
         (target (map-elt selectables target-key)))
    (if target
        (niri--target-action-internal target)
      (switch-to-buffer target-key nil t))))


(defun niri--target-action-external (target frames)
  "Focus the given TARGET (a Niri window or Emacs buffer). This
always behaves correctly, but does more work than the -internal
variant. It should only be called when invoking the switcher from
outside of Emacs (i.e. through `emacsclient').

FRAMES is the exact list of Emacs frames that existed at the time
the switcher was invoked."
  (pcase (car target)
    (:niri (niri--focus-window (cdr target)))

    ;; When switching to an Emacs buffer from outside of Emacs, we run into the
    ;; additional complication that Wayland does not allow arbitrary
    ;; applications to change the focused window. Calling e.g.
    ;; `select-frame-set-input-focus' has no effect on Wayland when not called
    ;; from within a focused Emacs frame.
    ;;
    ;; However, due to concurrency, frames may change between the moment when we
    ;; start the switcher (and potentially wait for user input), and when the
    ;; final selection happens.
    ;;
    ;; To get around this we try to match the target Emacs frame (if present) to
    ;; a Niri window, switch to it optimistically, and *then* execute the final
    ;; buffer switching command.
    (:emacs
     (if-let ((window (get-buffer-window (cdr target) t))
              (frame (window-frame window))
              (frame-name (frame-parameter frame 'name))
              (niri-window (seq-find (lambda (w)
                                       (equal (map-elt w "title") frame-name))
                                     frames)))
         ;; Target frame found and could be matched to a Niri window: Go there!
         (progn (select-window window) ;; ensure the right window in the frame has focus
                (niri--focus-window niri-window)
                (message "Switched to existing window for \"%s\"" (buffer-name (cdr target))))

       ;; Target frame not found; is Emacs the focused program?
       (if (seq-find (lambda (w) (map-elt w "is_focused")) frames)
           (switch-to-buffer (cdr target))
         ;; if not, just make a new frame
         (display-buffer (cdr target) '(display-buffer-pop-up-frame)))))))

(defun niri-go-anywhere-external ()
  "Use a dmenu-compatible launcher like `fuzzel' to achieve the same
effect as `niri-go-anywhere', but from outside of Emacs through
Emacsclient."
  (interactive) ;; TODO no?
  (let* ((all (niri--list-selectables))
         (selectables (car all))
         (target (with-temp-buffer
                   (dolist (key (map-keys selectables))
                     (insert key "\n"))
                   (call-process-region nil nil "fuzzel" t t nil "-d")
                   (string-trim (buffer-string)))))
    (when-let ((selectable (map-elt selectables target)))
      (niri--target-action-external selectable (cdr all)))))

(provide 'niri)
