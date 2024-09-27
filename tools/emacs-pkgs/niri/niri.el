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
   (shell-command-to-string "niri msg -j windows")))

(defun niri--list-selectables ()
  "Lists all currently selectable things in a format that can work
with completing-read. Selectable means all open Niri windows and
all Emacs buffers."
  (let* (;; all niri windows except for emacs frames
         (windows (seq-filter (lambda (w) (not (equal (map-elt w "app_id") "emacs")))
                              (niri-list-windows)))

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

    selectables))

(defun niri-go-anywhere ()
  "Interactively select and switch to an open Niri window, or an
  Emacs buffer."
  (interactive)
  (let* ((selectables (niri--list-selectables))
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

    (pcase (car target)
      (:emacs (pop-to-buffer (cdr target) '((display-buffer-reuse-window
                                             display-buffer-same-window)
                                            (reusable-frames . 0))))
      (:niri
       (shell-command (format "niri msg action focus-window --id %d"
                              (map-elt (cdr target) "id")))))))
