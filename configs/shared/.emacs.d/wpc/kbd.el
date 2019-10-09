;;; kbd.el --- Elisp keybinding -*- lexical-binding: t -*-
;; Author: William Carroll <wpcarro@gmail.com>

;;; Commentary:
;; In order to stay organized, I'm attempting to dedicate KBD prefixes to
;; specific functions.  I'm hoping I can be more deliberate with my keybinding
;; choices this way.
;;
;; Terminology:
;; For a more thorough overview of the terminology refer to `keybindings.md'
;; file.  Here's a brief overview:
;; - workspace: Anything concerning EXWM workspaces.
;; - x11: Anything concerning X11 applications.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dependencies
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'prelude)
(require 'alist)
(require 'set)
(require 'string)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst kbd/install-kbds? t
  "When t, install keybindings defined herein.")

(defconst kbd/prefixes
  '((workspace . "s")
    (x11 . "C-s"))
  "Mapping of functions to designated keybinding prefixes to stay organized.")

;; Assert that no keybindings are colliding.
(prelude/assert
 (= (alist/count kbd/prefixes)
    (->> kbd/prefixes
         alist/values
         set/from-list
         set/count)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Library
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun kbd/raw (f x)
  "Return the string keybinding for function F and appendage X.
Values for F include:
- workspace
- x11"
  (prelude/assert (alist/has-key? f kbd/prefixes))
  (string/format
   "%s-%s"
   (alist/get f kbd/prefixes)
   x))

(defun kbd/for (f x)
  "Return the `kbd' for function F and appendage X.
Values for F include:
- workspace
- x11"
  (kbd (kbd/raw f x)))

;; TODO: Prefer copying human-readable versions to the clipboard.  Right now
;; this isn't too useful.
(defun kbd/copy-keycode ()
  "Copy the pressed key to the system clipboard."
  (interactive)
  (message "[kbd] Awaiting keypress...")
  (let ((key (read-key)))
    (clipboard/copy (string/format "%s" key))
    (message (string/format "[kbd] \"%s\" copied!" key))))

(defun kbd/print-keycode ()
  "Prints the pressed keybinding."
  (interactive)
  (message "[kbd] Awaiting keypress...")
  (message (string/format "[kbd] keycode: %s" (read-key))))

;; (when kbd/install-kbds?
;;   (general-define-key
;;    :prefix "<SPC>"
;;    "hr" #'kbd/print-keycode))

(provide 'kbd)
;;; kbd.el ends here
