;;; entr.el --- Working with terminals and entr -*- lexical-binding: t -*-
;; Author: William Carroll <wpcarro@gmail.com>

;;; Commentary:
;; Help make watch commands easier.
;;
;; This should be entirely temporary because in reality we should be able to use
;; Emacs's buffer watching abilities to run commands.
;; TODO: Explore Emacs integration that obviates `entr`.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dependencies
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'f)
(require 'buffer)
(require 'prelude)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Library
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: Support a generic file-watcher for commonly used languages.
(defconst entr/major-mode->save-handler
  '((python-mode . entr/handle-python3))
  "Mapping of language to the `after-save-hook' function it should register.")

(defun entr/shell-command-to-buffer (cmd name)
  "Run CMD in a shell and output to the buffer NAME.
The buffer is a find-or-create operation.
The buffer is erased between runs with `erase-buffer'."
  (let ((b (buffer/find-or-create name)))
    (with-current-buffer b (erase-buffer))
    (shell-command cmd b)
    (buffer/show b)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Python
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: This should be a top-level function.
(defconst entr/handle-python3
  (lambda ()
    (entr/shell-command-to-buffer
     (format "python3 %s" (buffer-file-name))
     "*python3*"))
  "Function that is registered as the `after-save-hook' for python3.")

(defun entr/register-python3 ()
  "Register a buffer-local `after-save-hook' for calling python3 with filename."
  (interactive)
  (add-hook 'after-save-hook entr/handle-python3 nil t))

(defun entr/deregister-python3 ()
  "Remove the buffer-local `after-save-hook' for python3."
  (interactive)
  (remove-hook 'after-save-hook entr/handle-python3 t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Protobuf
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun entr/format-protobuf ()
  "Formats a protobuf buffer."
  (call-interactively #'clang-format))

;; TODO: Run this automatically with .proto file extensions.  Do this after
;; verifying that `clang-format' complies with Google's style guide.
(defun entr/register-protobuf ()
  "Register a buffer-local `before-save-hook' for formatting protobuf buffers."
  (interactive)
  (add-hook
   'before-save-hook
   #'entr/format-protobuf
   nil
   t))

;; TODO: Is there an interactive way to remove hooks in Emacs?
(defun entr/deregister-protobuf ()
  "Remove the buffer-local `before-save-hook' for protobuf."
  (interactive)
  (remove-hook
   'before-save-hook
   #'entr/format-protobuf
   t))

;; TODO: Support this.  Currently the `intern' call is the problem.
;; (defun entr/ivy-remove-hook (hook)
;;   "Use Counsel to remove a handler from HOOK."
;;   (interactive)
;;   (ivy-read
;;    "Remove hook: "
;;    (intern (prelude/prompt "Hook name: "))
;;    :action (lambda (x) (message x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Miscellaneous
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun entr/command (command)
  "Create a terminal instance with entr running COMMAND.
COMMAND is a function that is called with the current filename."
  ;; Algorithm:
  ;; - Get buffer's filename.
  ;; - Open terminator running: `echo entr <filename> | entr <command>`.
  (interactive)
  (with-current-buffer (current-buffer)
      (let ((filename (buffer-file-name)))
        (prelude/inspect
         (format "echo %s | entr %s" filename (funcall command filename))))))

(provide 'entr)
;;; entr.el ends here
