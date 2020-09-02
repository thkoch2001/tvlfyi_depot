;;; ivy-helpers.el --- More interfaces to ivy -*- lexical-binding: t -*-

;; Author: William Carroll <wpcarro@gmail.com>
;; Version: 0.0.1
;; URL: https://git.wpcarro.dev/wpcarro/briefcase
;; Package-Requires: ((emacs "24.3"))

;;; Commentary:
;; Hopefully to improve my workflows.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dependencies
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'tuple)
(require 'string)
(require 'cl-macs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Library
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl-defun ivy-helpers-kv (prompt kv f)
  "PROMPT users with the keys in KV and return its corresponding value.  Calls F
with the key and value from KV."
  (ivy-read
   prompt
   kv
   :require-match t
   :action (lambda (entry)
             (funcall f (car entry) (cdr entry)))))

(defun ivy-helpers-do-run-external-command (cmd)
  "Execute the specified CMD and notify the user when it finishes."
  (message "Starting %s..." cmd)
  (set-process-sentinel
   (start-process-shell-command cmd nil cmd)
   (lambda (process event)
     (when (string= event "finished\n")
       (message "%s process finished." process)))))

(defun ivy-helpers-list-external-commands ()
  "Create a list of all external commands available on $PATH."
  (cl-loop
   for dir in (split-string (getenv "PATH") path-separator)
   when (and (file-exists-p dir) (file-accessible-directory-p dir))
   for lsdir = (cl-loop for i in (directory-files dir t)
                        for bn = (file-name-nondirectory i)
                        when (and (not (s-contains? "-wrapped" i))
                                  (not (member bn completions))
                                  (not (file-directory-p i))
                                  (file-executable-p i))
                        collect bn)
   append lsdir into completions
   finally return (sort completions 'string-lessp)))

(defun ivy-helpers-run-external-command ()
  "Prompts the user with a list of all installed applications to launch."
  (interactive)
  (let ((external-commands-list (ivy-helpers-list-external-commands)))
    (ivy-read "Command:" external-commands-list
              :require-match t
              :action #'ivy-helpers-do-run-external-command)))

;;; Code:
(provide 'ivy-helpers)
;;; ivy-helpers.el ends here
