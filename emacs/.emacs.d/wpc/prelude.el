;;; prelude.el --- My attempt at augmenting Elisp stdlib -*- lexical-binding: t -*-
;; Author: William Carroll <wpcarro@gmail.com>

;;; Commentary:
;; Some of these ideas are scattered across other modules like `fs',
;; `string-functions', etc.  I'd like to keep everything modular.  I still don't
;; have an answer for which items belond in `misc'; I don't want that to become
;; a dumping grounds.  Ideally this file will `require' all other modules and
;; define just a handful of functions.

;; TODO: Consider removing all dependencies from prelude.el.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Third-party libraries
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 's)
(require 'dash)
(require 'f)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Libraries
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: Maybe don't globally import everything here.  Disable these and attepmt
;; to reload Emacs to assess damage.
(require 'string)
(require 'list)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun prelude/to-string (x)
  "Convert X to a string."
  (format "%s" x))

(defun prelude/inspect (&rest args)
  "Message `ARGS' where ARGS are any type."
  (->> args
       (list/map #'prelude/to-string)
       (apply #'string/concat)
       message))

(defmacro prelude/call-process-to-string (cmd &rest args)
  "Return the string output of CMD called with ARGS."
  `(with-temp-buffer
     (call-process ,cmd nil (current-buffer) nil ,@args)
     (buffer-string)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Assertions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: Should I `throw' instead of `error' here?
(defmacro prelude/assert (x)
  "Errors unless X is t.
These are strict assertions and purposely do not rely on truthiness."
  (let ((as-string (prelude/to-string x)))
    `(unless (equal t ,x)
       (error (string/concat "Assertion failed: " ,as-string)))))

(defmacro prelude/refute (x)
  "Errors unless X is nil."
  (let ((as-string (prelude/to-string x)))
    `(unless (equal nil ,x)
       (error (string/concat "Refutation failed: " ,as-string)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Adapter functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun prelude/identity (x)
  "Return X unchanged."
  x)

(defun prelude/const (x)
  "Return a variadic lambda that will return X."
  (lambda (&rest _) x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Miscellaneous
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: Consider packaging these into a linum-color.el package.
;; TODO: Generate the color used here from the theme.
(defvar linum/safe? nil
  "Flag indicating whether or not it is safe to work with `linum-mode'.")

(defvar linum/mru-color nil
  "Stores the color most recently attempted to be applied.")

(add-hook 'linum-mode-hook
          (lambda ()
            (setq linum/safe? t)
            (when (maybe/some? linum/mru-color)
              (set-face-foreground 'linum linum/mru-color))))

(defun prelude/set-line-number-color (color)
  "Safely set linum color to `COLOR'.

If this is called before Emacs initializes, the color will be stored in
`linum/mru-color' and applied once initialization completes.

Why is this safe?
If `(set-face-foreground 'linum)' is called before initialization completes,
Emacs will silently fail.  Without this function, it is easy to introduce
difficult to troubleshoot bugs in your init files."
  (if linum/safe?
      (set-face-foreground 'linum color)
    (setq linum/mru-color color)))

(defun prelude/prompt (prompt)
  "Read input from user with PROMPT."
  (read-string prompt))

(cl-defun prelude/start-process (&key name command)
  "Pass command string, COMMAND, and the function name, NAME.
This is a wrapper around `start-process' that has an API that resembles
`shell-command'."
  ;; TODO: Fix the bug with tokenizing here, since it will split any whitespace
  ;; character, even though it shouldn't in the case of quoted string in shell.
  ;; e.g. - "xmodmap -e 'one two three'" => '("xmodmap" "-e" "'one two three'")
  (prelude/refute (string/contains? "'" command))
  (let* ((tokens (string/split " " command))
         (program-name (list/head tokens))
         (program-args (list/tail tokens)))
    (apply #'start-process
           `(,(string/format "*%s<%s>*" program-name name)
             ,nil
             ,program-name
             ,@program-args))))

(defun prelude/executable-exists? (name)
  "Return t if CLI tool NAME exists according to `exec-path'."
  (let ((file (locate-file name exec-path)))
    (require 'maybe)
    (if (maybe/some? file)
        (f-exists? file)
      nil)))

(defmacro prelude/time (x)
  "Print the time it takes to evaluate X."
  `(benchmark 1 ',x))

(provide 'prelude)
;;; prelude.el ends here
