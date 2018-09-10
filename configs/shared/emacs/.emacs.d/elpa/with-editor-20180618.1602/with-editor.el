;;; with-editor.el --- Use the Emacsclient as $EDITOR -*- lexical-binding: t -*-

;; Copyright (C) 2014-2018  The Magit Project Contributors
;;
;; You should have received a copy of the AUTHORS.md file.  If not,
;; see https://github.com/magit/with-editor/blob/master/AUTHORS.md.

;; Author: Jonas Bernoulli <jonas@bernoul.li>
;; Maintainer: Jonas Bernoulli <jonas@bernoul.li>

;; Package-Requires: ((emacs "24.4") (async "1.9"))
;; Keywords: tools
;; Homepage: https://github.com/magit/with-editor

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with Magit.  If not, see http://www.gnu.org/licenses.

;;; Commentary:

;; This library makes it possible to reliably use the Emacsclient as
;; the `$EDITOR' of child processes.  It makes sure that they know how
;; to call home.  For remote processes a substitute is provided, which
;; communicates with Emacs on standard output/input instead of using a
;; socket as the Emacsclient does.

;; It provides the commands `with-editor-async-shell-command' and
;; `with-editor-shell-command', which are intended as replacements
;; for `async-shell-command' and `shell-command'.  They automatically
;; export `$EDITOR' making sure the executed command uses the current
;; Emacs instance as "the editor".  With a prefix argument these
;; commands prompt for an alternative environment variable such as
;; `$GIT_EDITOR'.  To always use these variants add this to your init
;; file:
;;
;;   (define-key (current-global-map)
;;     [remap async-shell-command] 'with-editor-async-shell-command)
;;   (define-key (current-global-map)
;;     [remap shell-command] 'with-editor-shell-command)

;; Alternatively use the global `shell-command-with-editor-mode',
;; which always sets `$EDITOR' for all Emacs commands which ultimately
;; use `shell-command' to asynchronously run some shell command.

;; The command `with-editor-export-editor' exports `$EDITOR' or
;; another such environment variable in `shell-mode', `term-mode' and
;; `eshell-mode' buffers.  Use this Emacs command before executing a
;; shell command which needs the editor set, or always arrange for the
;; current Emacs instance to be used as editor by adding it to the
;; appropriate mode hooks:
;;
;;   (add-hook 'shell-mode-hook  'with-editor-export-editor)
;;   (add-hook 'term-exec-hook   'with-editor-export-editor)
;;   (add-hook 'eshell-mode-hook 'with-editor-export-editor)

;; Some variants of this function exist, these two forms are
;; equivalent:
;;
;;   (add-hook 'shell-mode-hook
;;             (apply-partially 'with-editor-export-editor "GIT_EDITOR"))
;;   (add-hook 'shell-mode-hook 'with-editor-export-git-editor)

;; This library can also be used by other packages which need to use
;; the current Emacs instance as editor.  In fact this library was
;; written for Magit and its `git-commit-mode' and `git-rebase-mode'.
;; Consult `git-rebase.el' and the related code in `magit-sequence.el'
;; for a simple example.

;;; Code:

(require 'cl-lib)
;; `pcase-dolist' is not autoloaded on Emacs 24.
(eval-when-compile (require 'pcase))
(require 'server)
(require 'shell)

(and (require 'async-bytecomp nil t)
     (memq 'magit (bound-and-true-p async-bytecomp-allowed-packages))
     (fboundp 'async-bytecomp-package-mode)
     (async-bytecomp-package-mode 1))

(eval-when-compile
  (progn (require 'dired nil t)
         (require 'eshell nil t)
         (require 'term nil t)
         (require 'warnings nil t)))
(declare-function dired-get-filename 'dired)
(declare-function term-emulate-terminal 'term)
(defvar eshell-preoutput-filter-functions)

;;; Options

(defgroup with-editor nil
  "Use the Emacsclient as $EDITOR."
  :group 'external
  :group 'server)

(defun with-editor-locate-emacsclient ()
  "Search for a suitable Emacsclient executable."
  (or (with-editor-locate-emacsclient-1
       (with-editor-emacsclient-path)
       (length (split-string emacs-version "\\.")))
      (prog1 nil (display-warning 'with-editor "\
Cannot determine a suitable Emacsclient

Determining an Emacsclient executable suitable for the
current Emacs instance failed.  For more information
please see https://github.com/magit/magit/wiki/Emacsclient."))))

(defun with-editor-locate-emacsclient-1 (path depth)
  (let* ((version-lst (cl-subseq (split-string emacs-version "\\.") 0 depth))
         (version-reg (concat "^" (mapconcat #'identity version-lst "\\."))))
    (or (locate-file-internal
         (if (equal (downcase invocation-name) "remacs")
             "remacsclient"
           "emacsclient")
         path
         (cl-mapcan
          (lambda (v) (cl-mapcar (lambda (e) (concat v e)) exec-suffixes))
          (nconc (and (boundp 'debian-emacs-flavor)
                      (list (format ".%s" debian-emacs-flavor)))
                 (cl-mapcon (lambda (v)
                              (setq v (mapconcat #'identity (reverse v) "."))
                              (list v (concat "-" v) (concat ".emacs" v)))
                            (reverse version-lst))
                 (list "" "-snapshot" ".emacs-snapshot")))
         (lambda (exec)
           (ignore-errors
             (string-match-p version-reg
                             (with-editor-emacsclient-version exec)))))
        (and (> depth 1)
             (with-editor-locate-emacsclient-1 path (1- depth))))))

(defun with-editor-emacsclient-version (exec)
  (let ((default-directory (file-name-directory exec)))
    (ignore-errors
      (cadr (split-string (car (process-lines exec "--version")))))))

(defun with-editor-emacsclient-path ()
  (let ((path exec-path))
    (when invocation-directory
      (push (directory-file-name invocation-directory) path)
      (let* ((linkname (expand-file-name invocation-name invocation-directory))
             (truename (file-chase-links linkname)))
        (unless (equal truename linkname)
          (push (directory-file-name (file-name-directory truename)) path)))
      (when (eq system-type 'darwin)
        (let ((dir (expand-file-name "bin" invocation-directory)))
          (when (file-directory-p dir)
            (push dir path)))
        (when (string-match-p "Cellar" invocation-directory)
          (let ((dir (expand-file-name "../../../bin" invocation-directory)))
            (when (file-directory-p dir)
              (push dir path))))))
    (cl-remove-duplicates path :test 'equal)))

(defcustom with-editor-emacsclient-executable (with-editor-locate-emacsclient)
  "The Emacsclient executable used by the `with-editor' macro."
  :group 'with-editor
  :type '(choice (string :tag "Executable")
                 (const  :tag "Don't use Emacsclient" nil)))

(defcustom with-editor-sleeping-editor "\
sh -c '\
echo \"WITH-EDITOR: $$ OPEN $0\"; \
sleep 604800 & sleep=$!; \
trap \"kill $sleep; exit 0\" USR1; \
trap \"kill $sleep; exit 1\" USR2; \
wait $sleep'"
  "The sleeping editor, used when the Emacsclient cannot be used.

This fallback is used for asynchronous processes started inside
the macro `with-editor', when the process runs on a remote machine
or for local processes when `with-editor-emacsclient-executable'
is nil (i.e. when no suitable Emacsclient was found, or the user
decided not to use it).

Where the latter uses a socket to communicate with Emacs' server,
this substitute prints edit requests to its standard output on
which a process filter listens for such requests.  As such it is
not a complete substitute for a proper Emacsclient, it can only
be used as $EDITOR of child process of the current Emacs instance.

Some shells do not execute traps immediately when waiting for a
child process, but by default we do use such a blocking child
process.

If you use such a shell (e.g. `csh' on FreeBSD, but not Debian),
then you have to edit this option.  You can either replace \"sh\"
with \"bash\" (and install that), or you can use the older, less
performant implementation:

  \"sh -c '\\
  echo \\\"WITH-EDITOR: $$ OPEN $0\\\"; \\
  trap \\\"exit 0\\\" USR1; \\
  trap \\\"exit 1\" USR2; \\
  while true; do sleep 1; done'\"

Note that this leads to a delay of up to a second.  The delay can
be shortened by replacing \"sleep 1\" with \"sleep 0.01\", or if your
implementation does not support floats, then by using `nanosleep'
instead."
  :group 'with-editor
  :type 'string)

(defcustom with-editor-finish-query-functions nil
  "List of functions called to query before finishing session.

The buffer in question is current while the functions are called.
If any of them returns nil, then the session is not finished and
the buffer is not killed.  The user should then fix the issue and
try again.  The functions are called with one argument.  If it is
non-nil then that indicates that the user used a prefix argument
to force finishing the session despite issues.  Functions should
usually honor that and return non-nil."
  :group 'with-editor
  :type 'hook)
(put 'with-editor-finish-query-functions 'permanent-local t)

(defcustom with-editor-cancel-query-functions nil
  "List of functions called to query before canceling session.

The buffer in question is current while the functions are called.
If any of them returns nil, then the session is not canceled and
the buffer is not killed.  The user should then fix the issue and
try again.  The functions are called with one argument.  If it is
non-nil then that indicates that the user used a prefix argument
to force canceling the session despite issues.  Functions should
usually honor that and return non-nil."
  :group 'with-editor
  :type 'hook)
(put 'with-editor-cancel-query-functions 'permanent-local t)

(defcustom with-editor-mode-lighter " WE"
  "The mode-line lighter of the With-Editor mode."
  :group 'with-editor
  :type '(choice (const :tag "No lighter" "") string))

(defvar with-editor-server-window-alist nil
  "Alist of filename patterns vs corresponding `server-window'.

Each element looks like (REGEXP . FUNCTION).  Files matching
REGEXP are selected using FUNCTION instead of the default in
`server-window'.

Note that when a package adds an entry here then it probably
has a reason to disrespect `server-window' and it likely is
not a good idea to change such entries.")

(defvar with-editor-file-name-history-exclude nil
  "List of regexps for filenames `server-visit' should not remember.
When a filename matches any of the regexps, then `server-visit'
does not add it to the variable `file-name-history', which is
used when reading a filename in the minibuffer.")

(defcustom with-editor-shell-command-use-emacsclient t
  "Whether to use the emacsclient when running shell commands.

This affects `with-editor-shell-command-async' and, if the input
ends with \"&\" `with-editor-shell-command' .

If `shell-command-with-editor-mode' is enabled, then it also
affects `shell-command-async' and, if the input ends with \"&\"
`shell-command'.

This is a temporary kludge that lets you choose between two
possible defects, the ones described in the issues #23 and #40.

When t, then use the emacsclient.  This has the disadvantage that
`with-editor-mode' won't be enabled because we don't know whether
this package was involved at all in the call to the emacsclient,
and when it is not, then we really should.  The problem is that
the emacsclient doesn't pass a long any environment variables to
the server.  This will hopefully be fixed in Emacs eventually.

When nil, then use the sleeping editor.  Because in this case we
know that this package is involved, we can enable the mode.  But
this makes it necessary that you invoke $EDITOR in shell scripts
like so:

  eval \"$EDITOR\" file

And some tools that do not handle $EDITOR properly also break."
  :package-version '(with-editor . "2.8.0")
  :group 'with-editor
  :type 'boolean)

;;; Mode Commands

(defvar with-editor-pre-finish-hook nil)
(defvar with-editor-pre-cancel-hook nil)
(defvar with-editor-post-finish-hook nil)
(defvar with-editor-post-finish-hook-1 nil)
(defvar with-editor-post-cancel-hook nil)
(defvar with-editor-post-cancel-hook-1 nil)
(defvar with-editor-cancel-alist nil)
(put 'with-editor-pre-finish-hook 'permanent-local t)
(put 'with-editor-pre-cancel-hook 'permanent-local t)
(put 'with-editor-post-finish-hook 'permanent-local t)
(put 'with-editor-post-cancel-hook 'permanent-local t)

(defvar with-editor-show-usage t)
(defvar with-editor-cancel-message nil)
(defvar with-editor-previous-winconf nil)
(make-variable-buffer-local 'with-editor-show-usage)
(make-variable-buffer-local 'with-editor-cancel-message)
(make-variable-buffer-local 'with-editor-previous-winconf)
(put 'with-editor-cancel-message 'permanent-local t)
(put 'with-editor-previous-winconf 'permanent-local t)

(defvar-local with-editor--pid nil "For internal use.")
(put 'with-editor--pid 'permanent-local t)

(defun with-editor-finish (force)
  "Finish the current edit session."
  (interactive "P")
  (when (run-hook-with-args-until-failure
         'with-editor-finish-query-functions force)
    (let ((with-editor-post-finish-hook-1
           (ignore-errors (delq t with-editor-post-finish-hook))))
      (run-hooks 'with-editor-pre-finish-hook)
      (with-editor-return nil)
      (accept-process-output nil 0.1)
      (run-hooks 'with-editor-post-finish-hook-1))))

(defun with-editor-cancel (force)
  "Cancel the current edit session."
  (interactive "P")
  (when (run-hook-with-args-until-failure
         'with-editor-cancel-query-functions force)
    (let ((message with-editor-cancel-message))
      (when (functionp message)
        (setq message (funcall message)))
      (let ((with-editor-post-cancel-hook-1
             (ignore-errors (delq t with-editor-post-cancel-hook)))
            (with-editor-cancel-alist nil))
        (run-hooks 'with-editor-pre-cancel-hook)
        (with-editor-return t)
        (accept-process-output nil 0.1)
        (run-hooks 'with-editor-post-cancel-hook-1))
      (message (or message "Canceled by user")))))

(defun with-editor-return (cancel)
  (let ((winconf with-editor-previous-winconf)
        (clients server-buffer-clients)
        (dir default-directory)
        (pid with-editor--pid))
    (remove-hook 'kill-buffer-query-functions
                 'with-editor-kill-buffer-noop t)
    (cond (cancel
           (save-buffer)
           (if clients
               (dolist (client clients)
                 (ignore-errors
                   (server-send-string client "-error Canceled by user"))
                 (delete-process client))
             ;; Fallback for when emacs was used as $EDITOR
             ;; instead of emacsclient or the sleeping editor.
             ;; See https://github.com/magit/magit/issues/2258.
             (ignore-errors (delete-file buffer-file-name))
             (kill-buffer)))
          (t
           (save-buffer)
           (if clients
               ;; Don't use `server-edit' because we do not want to
               ;; show another buffer belonging to another client.
               ;; See https://github.com/magit/magit/issues/2197.
               (server-done)
             (kill-buffer))))
    (when pid
      (let ((default-directory dir))
        (process-file "kill" nil nil nil
                      "-s" (if cancel "USR2" "USR1") pid)))
    (when (and winconf (eq (window-configuration-frame winconf)
                           (selected-frame)))
      (set-window-configuration winconf))))

;;; Mode

(defvar with-editor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-c"                           'with-editor-finish)
    (define-key map [remap server-edit]                  'with-editor-finish)
    (define-key map [remap evil-save-and-close]          'with-editor-finish)
    (define-key map [remap evil-save-modified-and-close] 'with-editor-finish)
    (define-key map "\C-c\C-k"                           'with-editor-cancel)
    (define-key map [remap kill-buffer]                  'with-editor-cancel)
    (define-key map [remap ido-kill-buffer]              'with-editor-cancel)
    (define-key map [remap iswitchb-kill-buffer]         'with-editor-cancel)
    (define-key map [remap evil-quit]                    'with-editor-cancel)
    map))

(define-minor-mode with-editor-mode
  "Edit a file as the $EDITOR of an external process."
  :lighter with-editor-mode-lighter
  ;; Protect the user from killing the buffer without using
  ;; either `with-editor-finish' or `with-editor-cancel',
  ;; and from removing the key bindings for these commands.
  (unless with-editor-mode
    (user-error "With-Editor mode cannot be turned off"))
  (add-hook 'kill-buffer-query-functions
            'with-editor-kill-buffer-noop nil t)
  ;; `server-execute' displays a message which is not
  ;; correct when using this mode.
  (when with-editor-show-usage
    (with-editor-usage-message)))

(put 'with-editor-mode 'permanent-local t)

(defun with-editor-kill-buffer-noop ()
  (user-error (substitute-command-keys "\
Don't kill this buffer.  Instead cancel using \\[with-editor-cancel]")))

(defun with-editor-usage-message ()
  ;; Run after `server-execute', which is run using
  ;; a timer which starts immediately.
  (run-with-timer
   0.01 nil `(lambda ()
               (with-current-buffer ,(current-buffer)
                 (message (substitute-command-keys "\
Type \\[with-editor-finish] to finish, \
or \\[with-editor-cancel] to cancel"))))))

;;; Wrappers

(defvar with-editor--envvar nil "For internal use.")

(defmacro with-editor (&rest body)
  "Use the Emacsclient as $EDITOR while evaluating BODY.
Modify the `process-environment' for processes started in BODY,
instructing them to use the Emacsclient as $EDITOR.  If optional
ENVVAR is provided then bind that environment variable instead.
\n(fn [ENVVAR] BODY...)"
  (declare (indent defun) (debug (body)))
  `(let ((with-editor--envvar ,(if (stringp (car body))
                                   (pop body)
                                 '(or with-editor--envvar "EDITOR")))
         (process-environment process-environment))
     (with-editor--setup)
     ,@body))

(defun with-editor--setup ()
  (if (or (not with-editor-emacsclient-executable)
          (file-remote-p default-directory))
      (push (concat with-editor--envvar "=" with-editor-sleeping-editor)
            process-environment)
    ;; Make sure server-use-tcp's value is valid.
    (unless (featurep 'make-network-process '(:family local))
      (setq server-use-tcp t))
    ;; Make sure the server is running.
    (unless (process-live-p server-process)
      (when (server-running-p server-name)
        (setq server-name (format "server%s" (emacs-pid)))
        (when (server-running-p server-name)
          (server-force-delete server-name)))
      (server-start))
    ;; Tell $EDITOR to use the Emacsclient.
    (push (concat with-editor--envvar "="
                  (shell-quote-argument with-editor-emacsclient-executable)
                  ;; Tell the process where the server file is.
                  (and (not server-use-tcp)
                       (concat " --socket-name="
                               (shell-quote-argument
                                (expand-file-name server-name
                                                  server-socket-dir)))))
          process-environment)
    (when server-use-tcp
      (push (concat "EMACS_SERVER_FILE="
                    (expand-file-name server-name server-auth-dir))
            process-environment))
    ;; As last resort fallback to the sleeping editor.
    (push (concat "ALTERNATE_EDITOR=" with-editor-sleeping-editor)
          process-environment)))

(defun with-editor-server-window ()
  (or (and buffer-file-name
           (cdr (cl-find-if (lambda (cons)
                              (string-match-p (car cons) buffer-file-name))
                            with-editor-server-window-alist)))
      server-window))

(defun server-switch-buffer--with-editor-server-window-alist
    (fn &optional next-buffer killed-one filepos)
  "Honor `with-editor-server-window-alist' (which see)."
  (let ((server-window (with-current-buffer
                           (or next-buffer (current-buffer))
                         (when with-editor-mode
                           (setq with-editor-previous-winconf
                                 (current-window-configuration)))
                         (with-editor-server-window))))
    (funcall fn next-buffer killed-one filepos)))

(advice-add 'server-switch-buffer :around
            'server-switch-buffer--with-editor-server-window-alist)

(defun start-file-process--with-editor-process-filter
    (fn name buffer program &rest program-args)
  "When called inside a `with-editor' form and the Emacsclient
cannot be used, then give the process the filter function
`with-editor-process-filter'.  To avoid overriding the filter
being added here you should use `with-editor-set-process-filter'
instead of `set-process-filter' inside `with-editor' forms.

When the `default-directory' is located on a remote machine,
then also manipulate PROGRAM and PROGRAM-ARGS in order to set
the appropriate editor environment variable."
  (if (not with-editor--envvar)
      (apply fn name buffer program program-args)
    (when (file-remote-p default-directory)
      (unless (equal program "env")
        (push program program-args)
        (setq program "env"))
      (push (concat with-editor--envvar "=" with-editor-sleeping-editor)
            program-args))
    (let ((process (apply fn name buffer program program-args)))
      (set-process-filter process 'with-editor-process-filter)
      (process-put process 'default-dir default-directory)
      process)))

(advice-add 'start-file-process :around
            'start-file-process--with-editor-process-filter)

(defun with-editor-set-process-filter (process filter)
  "Like `set-process-filter' but keep `with-editor-process-filter'.
Give PROCESS the new FILTER but keep `with-editor-process-filter'
if that was added earlier by the adviced `start-file-process'.

Do so by wrapping the two filter functions using a lambda, which
becomes the actual filter.  It calls `with-editor-process-filter'
first, passing t as NO-STANDARD-FILTER.  Then it calls FILTER,
which may or may not insert the text into the PROCESS' buffer."
  (set-process-filter
   process
   (if (eq (process-filter process) 'with-editor-process-filter)
       `(lambda (proc str)
          (,filter proc str)
          (with-editor-process-filter proc str t))
     filter)))

(defvar with-editor-filter-visit-hook nil)

(defun with-editor-output-filter (string)
  (save-match-data
    (if (string-match "^WITH-EDITOR: \\([0-9]+\\) OPEN \\(.+?\\)\r?$" string)
        (let ((pid  (match-string 1 string))
              (file (match-string 2 string)))
          (with-current-buffer
              (find-file-noselect
               (if (and (file-name-absolute-p file) default-directory)
                   (concat (file-remote-p default-directory) file)
                 (expand-file-name file)))
            (with-editor-mode 1)
            (setq with-editor--pid pid)
            (run-hooks 'with-editor-filter-visit-hook)
            (funcall (or (with-editor-server-window) 'switch-to-buffer)
                     (current-buffer))
            (kill-local-variable 'server-window))
          nil)
      string)))

(defun with-editor-process-filter
    (process string &optional no-default-filter)
  "Listen for edit requests by child processes."
  (let ((default-directory (process-get process 'default-dir)))
    (with-editor-output-filter string))
  (unless no-default-filter
    (internal-default-process-filter process string)))

(advice-add 'server-visit-files :after
            'server-visit-files--with-editor-file-name-history-exclude)

(defun server-visit-files--with-editor-file-name-history-exclude
    (files _proc &optional _nowait)
  (pcase-dolist (`(,file . ,_) files)
    (when (cl-find-if (lambda (regexp)
                        (string-match-p regexp file))
                      with-editor-file-name-history-exclude)
      (setq file-name-history (delete file file-name-history)))))

;;; Augmentations

;;;###autoload
(cl-defun with-editor-export-editor (&optional (envvar "EDITOR"))
  "Teach subsequent commands to use current Emacs instance as editor.

Set and export the environment variable ENVVAR, by default
\"EDITOR\".  The value is automatically generated to teach
commands to use the current Emacs instance as \"the editor\".

This works in `shell-mode', `term-mode' and `eshell-mode'."
  (interactive (list (with-editor-read-envvar)))
  (cond
   ((derived-mode-p 'comint-mode 'term-mode)
    (let ((process (get-buffer-process (current-buffer))))
      (goto-char (process-mark process))
      (process-send-string
       process (format " export %s=%s\n" envvar
                       (shell-quote-argument with-editor-sleeping-editor)))
      (while (accept-process-output process 0.1))
      (if (derived-mode-p 'term-mode)
          (with-editor-set-process-filter process 'with-editor-emulate-terminal)
        (add-hook 'comint-output-filter-functions 'with-editor-output-filter
                  nil t))))
   ((derived-mode-p 'eshell-mode)
    (add-to-list 'eshell-preoutput-filter-functions
                 'with-editor-output-filter)
    (setenv envvar with-editor-sleeping-editor))
   (t
    (error "Cannot export environment variables in this buffer")))
  (message "Successfully exported %s" envvar))

;;;###autoload
(defun with-editor-export-git-editor ()
  "Like `with-editor-export-editor' but always set `$GIT_EDITOR'."
  (interactive)
  (with-editor-export-editor "GIT_EDITOR"))

;;;###autoload
(defun with-editor-export-hg-editor ()
  "Like `with-editor-export-editor' but always set `$HG_EDITOR'."
  (interactive)
  (with-editor-export-editor "HG_EDITOR"))

(defun with-editor-emulate-terminal (process string)
  "Like `term-emulate-terminal' but also handle edit requests."
  (when (with-editor-output-filter string)
    (term-emulate-terminal process string)))

(defvar with-editor-envvars '("EDITOR" "GIT_EDITOR" "HG_EDITOR"))

(cl-defun with-editor-read-envvar
    (&optional (prompt  "Set environment variable")
               (default "EDITOR"))
  (let ((reply (completing-read (if default
                                    (format "%s (%s): " prompt default)
                                  (concat prompt ": "))
                                with-editor-envvars nil nil nil nil default)))
    (if (string= reply "") (user-error "Nothing selected") reply)))

;;;###autoload
(define-minor-mode shell-command-with-editor-mode
  "Teach `shell-command' to use current Emacs instance as editor.

Teach `shell-command', and all commands that ultimately call that
command, to use the current Emacs instance as editor by executing
\"EDITOR=CLIENT COMMAND&\" instead of just \"COMMAND&\".

CLIENT is automatically generated; EDITOR=CLIENT instructs
COMMAND to use to the current Emacs instance as \"the editor\",
assuming no other variable overrides the effect of \"$EDITOR\".
CLIENT may be the path to an appropriate emacsclient executable
with arguments, or a script which also works over Tramp.

Alternatively you can use the `with-editor-async-shell-command',
which also allows the use of another variable instead of
\"EDITOR\"."
  :global t)

;;;###autoload
(defun with-editor-async-shell-command
    (command &optional output-buffer error-buffer envvar)
  "Like `async-shell-command' but with `$EDITOR' set.

Execute string \"ENVVAR=CLIENT COMMAND\" in an inferior shell;
display output, if any.  With a prefix argument prompt for an
environment variable, otherwise the default \"EDITOR\" variable
is used.  With a negative prefix argument additionally insert
the COMMAND's output at point.

CLIENT is automatically generated; ENVVAR=CLIENT instructs
COMMAND to use to the current Emacs instance as \"the editor\",
assuming it respects ENVVAR as an \"EDITOR\"-like variable.
CLIENT may be the path to an appropriate emacsclient executable
with arguments, or a script which also works over Tramp.

Also see `async-shell-command' and `shell-command'."
  (interactive (with-editor-shell-command-read-args "Async shell command: " t))
  (let ((with-editor--envvar envvar))
    (with-editor
      (async-shell-command command output-buffer error-buffer))))

;;;###autoload
(defun with-editor-shell-command
    (command &optional output-buffer error-buffer envvar)
  "Like `shell-command' or `with-editor-async-shell-command'.
If COMMAND ends with \"&\" behave like the latter,
else like the former."
  (interactive (with-editor-shell-command-read-args "Shell command: "))
  (if (string-match "&[ \t]*\\'" command)
      (with-editor-async-shell-command
       command output-buffer error-buffer envvar)
    (shell-command command output-buffer error-buffer)))

(defun with-editor-shell-command-read-args (prompt &optional async)
  (let ((command (read-shell-command
                  prompt nil nil
                  (let ((filename (or buffer-file-name
                                      (and (eq major-mode 'dired-mode)
                                           (dired-get-filename nil t)))))
                    (and filename (file-relative-name filename))))))
    (list command
          (if (or async (setq async (string-match-p "&[ \t]*\\'" command)))
              (< (prefix-numeric-value current-prefix-arg) 0)
            current-prefix-arg)
          shell-command-default-error-buffer
          (and async current-prefix-arg (with-editor-read-envvar)))))

(defun shell-command--shell-command-with-editor-mode
    (fn command &optional output-buffer error-buffer)
  ;; `shell-mode' and its hook are intended for buffers in which an
  ;; interactive shell is running, but `shell-command' also turns on
  ;; that mode, even though it only runs the shell to run a single
  ;; command.  The `with-editor-export-editor' hook function is only
  ;; intended to be used in buffers in which an interactive shell is
  ;; running, so it has to be remove here.
  (let ((shell-mode-hook (remove 'with-editor-export-editor shell-mode-hook)))
    (cond ((or (not (or with-editor--envvar shell-command-with-editor-mode))
               (not (string-match-p "&\\'" command)))
           (funcall fn command output-buffer error-buffer))
          ((and with-editor-shell-command-use-emacsclient
                with-editor-emacsclient-executable
                (not (file-remote-p default-directory)))
           (with-editor (funcall fn command output-buffer error-buffer)))
          (t
           (apply fn (format "%s=%s %s"
                             (or with-editor--envvar "EDITOR")
                             (shell-quote-argument with-editor-sleeping-editor)
                             command)
                  output-buffer error-buffer)
           (ignore-errors
             (let ((process (get-buffer-process
                             (or output-buffer
                                 (get-buffer "*Async Shell Command*")))))
               (set-process-filter
                process (lambda (proc str)
                          (comint-output-filter proc str)
                          (with-editor-process-filter proc str t)))
               process))))))

(advice-add 'shell-command :around
            'shell-command--shell-command-with-editor-mode)

;;; _

(defun with-editor-debug ()
  "Debug configuration issues.
See info node `(with-editor)Debugging' for instructions."
  (interactive)
  (with-current-buffer (get-buffer-create "*with-editor-debug*")
    (pop-to-buffer (current-buffer))
    (erase-buffer)
    (ignore-errors (with-editor))
    (insert
     (format "with-editor: %s\n" (locate-library "with-editor.el"))
     (format "emacs: %s (%s)\n"
             (expand-file-name invocation-name invocation-directory)
             emacs-version)
     "system:\n"
     (format "  system-type: %s\n" system-type)
     (format "  system-configuration: %s\n" system-configuration)
     (format "  system-configuration-options: %s\n" system-configuration-options)
     "server:\n"
     (format "  server-running-p: %s\n" (server-running-p))
     (format "  server-process: %S\n" server-process)
     (format "  server-use-tcp: %s\n" server-use-tcp)
     (format "  server-name: %s\n" server-name)
     (format "  server-socket-dir: %s\n" server-socket-dir))
    (if (and server-socket-dir (file-accessible-directory-p server-socket-dir))
        (dolist (file (directory-files server-socket-dir nil "^[^.]"))
          (insert (format "    %s\n" file)))
      (insert (format "    %s: not an accessible directory\n"
                      (if server-use-tcp "WARNING" "ERROR"))))
    (insert (format "  server-auth-dir: %s\n" server-auth-dir))
    (if (file-accessible-directory-p server-auth-dir)
        (dolist (file (directory-files server-auth-dir nil "^[^.]"))
          (insert (format "    %s\n" file)))
      (insert (format "    %s: not an accessible directory\n"
                      (if server-use-tcp "ERROR" "WARNING"))))
    (let ((val with-editor-emacsclient-executable)
          (def (default-value 'with-editor-emacsclient-executable))
          (fun (let ((warning-minimum-level :error)
                     (warning-minimum-log-level :error))
                 (with-editor-locate-emacsclient))))
      (insert "with-editor-emacsclient-executable:\n"
              (format " value:   %s (%s)\n" val
                      (and val (with-editor-emacsclient-version val)))
              (format " default: %s (%s)\n" def
                      (and def (with-editor-emacsclient-version def)))
              (format " funcall: %s (%s)\n" fun
                      (and fun (with-editor-emacsclient-version fun)))))
    (insert "path:\n"
            (format "  $PATH: %S\n" (getenv "PATH"))
            (format "  exec-path: %s\n" exec-path))
    (insert (format "  with-editor-emacsclient-path:\n"))
    (dolist (dir (with-editor-emacsclient-path))
      (insert (format "    %s (%s)\n" dir (car (file-attributes dir))))
      (when (file-directory-p dir)
        ;; Don't match emacsclientw.exe, it makes popup windows.
        (dolist (exec (directory-files dir t "emacsclient\\(?:[^w]\\|\\'\\)"))
          (insert (format "      %s (%s)\n" exec
                          (with-editor-emacsclient-version exec))))))))

(defconst with-editor-font-lock-keywords
  '(("(\\(with-\\(?:git-\\)?editor\\)\\_>" (1 'font-lock-keyword-face))))
(font-lock-add-keywords 'emacs-lisp-mode with-editor-font-lock-keywords)

(provide 'with-editor)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; with-editor.el ends here
