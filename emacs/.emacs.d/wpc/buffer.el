;;; buffer.el --- Working with Emacs buffers -*- lexical-binding: t -*-
;; Author: William Carroll <wpcarro@gmail.com>

;;; Commentary:
;; Utilities for CRUDing buffers in Emacs.
;;
;; Many of these functions may seem unnecessary especially when you consider
;; there implementations.  In general I believe that Elisp suffers from a
;; library disorganization problem.  Providing simple wrapper functions that
;; rename functions or reorder parameters is worth the effort in my opinion if
;; it improves discoverability (via intuition) and improve composability.
;;
;; I support three ways for switching between what I'm calling "source code
;; buffers":
;; 1. Toggling previous: <SPC><SPC>
;; 2. Using `ivy-read': <SPC>b
;; TODO: These obscure evil KBDs. Maybe a hydra definition would be best?
;; 3. Cycling (forwards/backwards): C-f, C-b

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dependencies
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'prelude)
(require 'maybe)
(require 'set)
(require 'cycle)
(require 'struct)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Library
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst buffer/enable-tests? t
  "When t, run the test suite.")

(defconst buffer/install-kbds? t
  "When t, install the keybindings defined herein.")

(defconst buffer/source-code-blacklist
  (set/new 'dired-mode
           'erc-mode
           'magit-status-mode
           'magit-process-mode
           'magit-log-mode
           'org-mode
           'fundamental-mode)
  "A blacklist of major-modes to ignore for listing source code buffers.")

(defconst buffer/source-code-timeout 2
  "Number of seconds to wait before invalidating the cycle.")

(cl-defstruct source-code-cycle cycle last-called)

(defun buffer/emacs-generated? (name)
  "Return t if buffer, NAME, is an Emacs-generated buffer.
Some buffers are Emacs-generated but are surrounded by whitespace."
  (let ((trimmed (s-trim name)))
    (and (s-starts-with? "*" trimmed))))

(defun buffer/find (buffer-or-name)
  "Find a buffer by its BUFFER-OR-NAME."
  (get-buffer buffer-or-name))

(defun buffer/major-mode (name)
  "Return the active `major-mode' in buffer, NAME."
  (with-current-buffer (buffer/find name)
    major-mode))

(defun buffer/source-code-buffers ()
  "Return a list of source code buffers.
This will ignore Emacs-generated buffers, like *Messages*.  It will also ignore
  any buffer whose major mode is defined in `buffer/source-code-blacklist'."
  (->> (buffer-list)
       (list/map #'buffer-name)
       (list/reject #'buffer/emacs-generated?)
       (list/reject (lambda (name)
                      (set/contains? (buffer/major-mode name)
                                     buffer/source-code-blacklist)))))

(defvar buffer/source-code-cycle-state
  (make-source-code-cycle
   :cycle (cycle/from-list (buffer/source-code-buffers))
   :last-called (ts-now))
  "State used to manage cycling between source code buffers.")

(defun buffer/exists? (name)
  "Return t if buffer, NAME, exists."
  (maybe/some? (buffer/find name)))

(defun buffer/new (name)
  "Return a newly created buffer NAME."
  (generate-new-buffer name))

(defun buffer/find-or-create (name)
  "Find or create buffer, NAME.
Return a reference to that buffer."
  (let ((x (buffer/find name)))
    (if (maybe/some? x)
        x
      (buffer/new name))))

;; TODO: Should this consume: `display-buffer' or `switch-to-buffer'?
(defun buffer/show (buffer-or-name)
  "Display the BUFFER-OR-NAME, which is either a buffer reference or its name."
  (display-buffer buffer-or-name))

;; TODO: Move this and `buffer/cycle-prev' into a separate module that
;; encapsulates all of this behavior.

(defun buffer/cycle (cycle-fn)
  "Cycle forwards or backwards through `buffer/source-code-buffers'."
  (let ((last-called (source-code-cycle-last-called
                      buffer/source-code-cycle-state))
        (cycle (source-code-cycle-cycle
                buffer/source-code-cycle-state)))
    (if (> (ts-diff (ts-now) last-called)
           buffer/source-code-timeout)
        (progn
          (struct/set! source-code-cycle
                       cycle
                       (cycle/from-list (buffer/source-code-buffers))
                       buffer/source-code-cycle-state)
          (let ((cycle (source-code-cycle-cycle
                        buffer/source-code-cycle-state)))
            (funcall cycle-fn cycle)
            (switch-to-buffer (cycle/current cycle)))
          (struct/set! source-code-cycle
                       last-called
                       (ts-now)
                       buffer/source-code-cycle-state))
      (progn
        (funcall cycle-fn cycle)
        (switch-to-buffer (cycle/current cycle))))))

(defun buffer/cycle-next ()
  "Cycle forward through the `buffer/source-code-buffers'."
  (interactive)
  (buffer/cycle #'cycle/next))

(defun buffer/cycle-prev ()
  "Cycle backward through the `buffer/source-code-buffers'."
  (interactive)
  (buffer/cycle #'cycle/prev))

(defun buffer/ivy-source-code ()
  "Use `ivy-read' to choose among all open source code buffers."
  (interactive)
  (ivy-read "Source code buffer: "
            (-drop 1 (buffer/source-code-buffers))
            :sort nil
            :action #'switch-to-buffer))

(defun buffer/show-previous ()
  "Call `switch-to-buffer' on the previously visited buffer.
This function ignores Emacs-generated buffers, i.e. the ones that look like
  this: *Buffer*.  It also ignores buffers that are `dired-mode' or `erc-mode'.
  This blacklist can easily be changed."
  (interactive)
  (let* ((xs (buffer/source-code-buffers))
         (candidate (list/get 1 xs)))
    (prelude/assert (maybe/some? candidate))
    (switch-to-buffer candidate)))

(when buffer/install-kbds?
  (general-define-key
   :states '(normal)
   "C-f" #'buffer/cycle-next
   "C-b" #'buffer/cycle-prev)
  (general-define-key
   :prefix "<SPC>"
   :states '(normal)
   "b" #'buffer/ivy-source-code
   "<SPC>" #'buffer/show-previous
   "k" #'kill-buffer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when buffer/enable-tests?
  (prelude/assert
   (list/all? #'buffer/emacs-generated?
              '("*scratch*"
                "*Messages*"
                "*shell*"
                "*Shell Command Output*"
                "*Occur*"
                "*Warnings*"
                "*Help*"
                "*Completions*"
                "*Apropos*"
                "*info*"))))

(provide 'buffer)
;;; buffer.el ends here
