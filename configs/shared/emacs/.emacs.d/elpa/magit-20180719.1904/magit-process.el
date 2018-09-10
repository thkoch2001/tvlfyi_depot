;;; magit-process.el --- process functionality  -*- lexical-binding: t -*-

;; Copyright (C) 2010-2018  The Magit Project Contributors
;;
;; You should have received a copy of the AUTHORS.md file which
;; lists all contributors.  If not, see http://magit.vc/authors.

;; Author: Jonas Bernoulli <jonas@bernoul.li>
;; Maintainer: Jonas Bernoulli <jonas@bernoul.li>

;; Magit is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; Magit is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Magit.  If not, see http://www.gnu.org/licenses.

;;; Commentary:

;; This library implements the tools used to run Git for side-effects.

;; Note that the functions used to run Git and then consume its
;; output, are defined in `magit-git.el'.  There's a bit of overlap
;; though.

;;; Code:

(require 'ansi-color)
(require 'cl-lib)
(require 'dash)

(require 'with-editor)
(require 'magit-utils)
(require 'magit-section)
(require 'magit-git)
(require 'magit-mode)

(declare-function auth-source-search "auth-source"
                  (&rest spec &key max require create delete &allow-other-keys))

;;; Options

(defcustom magit-process-connection-type (not (eq system-type 'cygwin))
  "Connection type used for the Git process.

If nil, use pipes: this is usually more efficient, and works on Cygwin.
If t, use ptys: this enables Magit to prompt for passphrases when needed."
  :group 'magit-process
  :type '(choice (const :tag "pipe" nil)
                 (const :tag "pty" t)))

(defcustom magit-need-cygwin-noglob
  (and (eq system-type 'windows-nt)
       (with-temp-buffer
         (let ((process-environment
                (append magit-git-environment process-environment)))
           (condition-case e
               (process-file magit-git-executable
                             nil (current-buffer) nil
                             "-c" "alias.echo=!echo" "echo" "x{0}")
             (file-error
              (lwarn 'magit-process :warning
                     "Could not run Git: %S" e))))
         (equal "x0\n" (buffer-string))))
  "Whether to use a workaround for Cygwin's globbing behavior.

If non-nil, add environment variables to `process-environment' to
prevent the git.exe distributed by Cygwin and MSYS2 from
attempting to perform glob expansion when called from a native
Windows build of Emacs.  See #2246."
  :package-version '(magit . "2.3.0")
  :group 'magit-process
  :type '(choice (const :tag "Yes" t)
                 (const :tag "No" nil)))

(defcustom magit-process-popup-time -1
  "Popup the process buffer if a command takes longer than this many seconds."
  :group 'magit-process
  :type '(choice (const :tag "Never" -1)
                 (const :tag "Immediately" 0)
                 (integer :tag "After this many seconds")))

(defcustom magit-process-log-max 32
  "Maximum number of sections to keep in a process log buffer.
When adding a new section would go beyond the limit set here,
then the older half of the sections are remove.  Sections that
belong to processes that are still running are never removed.
When this is nil, no sections are ever removed."
  :package-version '(magit . "2.1.0")
  :group 'magit-process
  :type '(choice (const :tag "Never remove old sections" nil) integer))

(defcustom magit-process-error-tooltip-max-lines 20
  "The number of lines for `magit-process-error-lines' to return.

These are displayed in a tooltip for `mode-line-process' errors.

If `magit-process-error-tooltip-max-lines' is nil, the tooltip
displays the text of `magit-process-error-summary' instead."
  :package-version '(magit . "2.12.0")
  :group 'magit-process
  :type '(choice (const :tag "Use summary line" nil)
                 integer))

(defcustom magit-credential-cache-daemon-socket
  (--some (pcase-let ((`(,prog . ,args) (split-string it)))
            (if (and prog
                     (string-match-p
                      "\\`\\(?:\\(?:/.*/\\)?git-credential-\\)?cache\\'" prog))
                (or (cl-loop for (opt val) on args
                             if (string= opt "--socket")
                             return val)
                    (expand-file-name "~/.git-credential-cache/socket"))))
          ;; Note: `magit-process-file' is not yet defined when
          ;; evaluating this form, so we use `process-lines'.
          (ignore-errors
            (let ((process-environment
                   (append magit-git-environment process-environment)))
              (process-lines magit-git-executable
                             "config" "--get-all" "credential.helper"))))
  "If non-nil, start a credential cache daemon using this socket.

When using Git's cache credential helper in the normal way, Emacs
sends a SIGHUP to the credential daemon after the git subprocess
has exited, causing the daemon to also quit.  This can be avoided
by starting the `git-credential-cache--daemon' process directly
from Emacs.

The function `magit-maybe-start-credential-cache-daemon' takes
care of starting the daemon if necessary, using the value of this
option as the socket.  If this option is nil, then it does not
start any daemon.  Likewise if another daemon is already running,
then it starts no new daemon.  This function has to be a member
of the hook variable `magit-credential-hook' for this to work.
If an error occurs while starting the daemon, most likely because
the necessary executable is missing, then the function removes
itself from the hook, to avoid further futile attempts."
  :package-version '(magit . "2.3.0")
  :group 'magit-process
  :type '(choice (file  :tag "Socket")
                 (const :tag "Don't start a cache daemon" nil)))

(defcustom magit-process-yes-or-no-prompt-regexp
  " [\[(]\\([Yy]\\(?:es\\)?\\)[/|]\\([Nn]o?\\)[\])] ?[?:] ?$"
  "Regexp matching Yes-or-No prompts of Git and its subprocesses."
  :package-version '(magit . "2.1.0")
  :group 'magit-process
  :type 'regexp)

(defcustom magit-process-password-prompt-regexps
  '("^\\(Enter \\)?[Pp]assphrase\\( for \\(RSA \\)?key '.*'\\)?: ?$"
    ;; match-group 99 is used to identify a host
    "^\\(Enter \\)?[Pp]assword\\( for '\\(?99:.*\\)'\\)?: ?$"
    "^.*'s password: ?$"
    "^Yubikey for .*: ?$"
    "^Enter PIN for .*: ?$")
  "List of regexps matching password prompts of Git and its subprocesses.
Also see `magit-process-find-password-functions'."
  :package-version '(magit . "2.8.0")
  :group 'magit-process
  :type '(repeat (regexp)))

(defcustom magit-process-find-password-functions nil
  "List of functions to try in sequence to get a password.

These functions may be called when git asks for a password, which
is detected using `magit-process-password-prompt-regexps'.  They
are called if and only if matching the prompt resulted in the
value of the 99th submatch to be non-nil.  Therefore users can
control for which prompts these functions should be called by
putting the host name in the 99th submatch, or not.

If the functions are called, then they are called in the order
given, with the host name as only argument, until one of them
returns non-nil.  If they are not called or none of them returns
non-nil, then the password is read from the user instead."
  :package-version '(magit . "2.3.0")
  :group 'magit-process
  :type 'hook
  :options '(magit-process-password-auth-source))

(defcustom magit-process-username-prompt-regexps
  '("^Username for '.*': ?$")
  "List of regexps matching username prompts of Git and its subprocesses."
  :package-version '(magit . "2.1.0")
  :group 'magit-process
  :type '(repeat (regexp)))

(defcustom magit-process-ensure-unix-line-ending t
  "Whether Magit should ensure a unix coding system when talking to Git."
  :package-version '(magit . "2.6.0")
  :group 'magit-process
  :type 'boolean)

(defcustom magit-process-display-mode-line-error t
  "Whether Magit should retain and highlight process errors in the mode line."
  :package-version '(magit . "2.12.0")
  :group 'magit-process
  :type 'boolean)

(defface magit-process-ok
  '((t :inherit magit-section-heading :foreground "green"))
  "Face for zero exit-status."
  :group 'magit-faces)

(defface magit-process-ng
  '((t :inherit magit-section-heading :foreground "red"))
  "Face for non-zero exit-status."
  :group 'magit-faces)

(defface magit-mode-line-process
  '((t :inherit mode-line-emphasis))
  "Face for `mode-line-process' status when Git is running for side-effects."
  :group 'magit-faces)

(defface magit-mode-line-process-error
  '((t :inherit error))
  "Face for `mode-line-process' error status.

Used when `magit-process-display-mode-line-error' is non-nil."
  :group 'magit-faces)

;;; Process Mode

(defvar magit-process-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map magit-mode-map)
    map)
  "Keymap for `magit-process-mode'.")

(define-derived-mode magit-process-mode magit-mode "Magit Process"
  "Mode for looking at Git process output."
  :group 'magit-process
  (hack-dir-local-variables-non-file-buffer)
  (setq imenu-prev-index-position-function
        'magit-imenu--process-prev-index-position-function)
  (setq imenu-extract-index-name-function
        'magit-imenu--process-extract-index-name-function))

(defun magit-process-buffer (&optional nodisplay)
  "Display the current repository's process buffer.

If that buffer doesn't exist yet, then create it.
Non-interactively return the buffer and unless
optional NODISPLAY is non-nil also display it."
  (interactive)
  (let ((topdir (magit-toplevel)))
    (unless topdir
      (magit--with-safe-default-directory nil
        (setq topdir default-directory)
        (let (prev)
          (while (not (equal topdir prev))
            (setq prev topdir)
            (setq topdir (file-name-directory (directory-file-name topdir)))))))
    (let ((buffer (or (--first (with-current-buffer it
                                 (and (eq major-mode 'magit-process-mode)
                                      (equal default-directory topdir)))
                               (buffer-list))
                      (let ((default-directory topdir))
                        (magit-generate-new-buffer 'magit-process-mode)))))
      (with-current-buffer buffer
        (if magit-root-section
            (when magit-process-log-max
              (magit-process-truncate-log))
          (magit-process-mode)
          (let ((inhibit-read-only t)
                (magit-insert-section--parent  nil)
                (magit-insert-section--oldroot nil))
            (make-local-variable 'text-property-default-nonsticky)
            (magit-insert-section (processbuf)
              (insert "\n")))))
      (unless nodisplay
        (magit-display-buffer buffer))
      buffer)))

(defun magit-process-kill ()
  "Kill the process at point."
  (interactive)
  (magit-section-when process
    (let ((process (oref it value)))
      (unless (eq (process-status process) 'run)
        (user-error "Process isn't running"))
      (magit-confirm 'kill-process)
      (kill-process process))))

;;; Synchronous Processes

(defvar magit-process-raise-error nil)

(defun magit-git (&rest args)
  "Call Git synchronously in a separate process, for side-effects.

Option `magit-git-executable' specifies the Git executable.
The arguments ARGS specify arguments to Git, they are flattened
before use.

Process output goes into a new section in the buffer returned by
`magit-process-buffer'.  If Git exits with a non-zero status,
then raise an error."
  (let ((magit-process-raise-error t))
    (magit-call-git args)))

(defun magit-run-git (&rest args)
  "Call Git synchronously in a separate process, and refresh.

Option `magit-git-executable' specifies the Git executable and
option `magit-git-global-arguments' specifies constant arguments.
The arguments ARGS specify arguments to Git, they are flattened
before use.

After Git returns, the current buffer (if it is a Magit buffer)
as well as the current repository's status buffer are refreshed.

Process output goes into a new section in the buffer returned by
`magit-process-buffer'."
  (let ((magit--refresh-cache (list (cons 0 0))))
    (magit-call-git args)
    (when (member (car args) '("init" "clone"))
      ;; Creating a new repository invalidates the cache.
      (setq magit--refresh-cache nil))
    (magit-refresh)))

(defvar magit-pre-call-git-hook nil)

(defun magit-call-git (&rest args)
  "Call Git synchronously in a separate process.

Option `magit-git-executable' specifies the Git executable and
option `magit-git-global-arguments' specifies constant arguments.
The arguments ARGS specify arguments to Git, they are flattened
before use.

Process output goes into a new section in the buffer returned by
`magit-process-buffer'."
  (run-hooks 'magit-pre-call-git-hook)
  (let ((default-process-coding-system (magit--process-coding-system)))
    (apply #'magit-call-process magit-git-executable
           (magit-process-git-arguments args))))

(defun magit-call-process (program &rest args)
  "Call PROGRAM synchronously in a separate process.
Process output goes into a new section in the buffer returned by
`magit-process-buffer'."
  (pcase-let ((`(,process-buf . ,section)
               (magit-process-setup program args)))
    (magit-process-finish
     (let ((inhibit-read-only t))
       (apply #'magit-process-file program nil process-buf nil args))
     process-buf (current-buffer) default-directory section)))

(defun magit-process-file (process &optional infile buffer display &rest args)
  "Process files synchronously in a separate process.
Identical to `process-file' but temporarily enable Cygwin's
\"noglob\" option during the call and ensure unix eol
conversion."
  (let ((process-environment (magit-process-environment))
        (default-process-coding-system (magit--process-coding-system)))
    (apply #'process-file process infile buffer display args)))

(defun magit-process-environment ()
  ;; The various w32 hacks are only applicable when running on the
  ;; local machine.  As of Emacs 25.1, a local binding of
  ;; process-environment different from the top-level value affects
  ;; the environment used in
  ;; tramp-sh-handle-{start-file-process,process-file}.
  (let ((local (not (file-remote-p default-directory))))
    (append magit-git-environment
            (and local
                 (cdr (assoc magit-git-executable magit-git-w32-path-hack)))
            (and local magit-need-cygwin-noglob
                 (mapcar (lambda (var)
                           (concat var "=" (--if-let (getenv var)
                                               (concat it " noglob")
                                             "noglob")))
                         '("CYGWIN" "MSYS")))
            process-environment)))

(defvar magit-this-process nil)

(defun magit-run-git-with-input (&rest args)
  "Call Git in a separate process.
ARGS is flattened and then used as arguments to Git.

The current buffer's content is used as the process' standard
input.

Option `magit-git-executable' specifies the Git executable and
option `magit-git-global-arguments' specifies constant arguments.
The remaining arguments ARGS specify arguments to Git, they are
flattened before use."
  (declare (indent 1))
  (when (eq system-type 'windows-nt)
    ;; On w32, git expects UTF-8 encoded input, ignore any user
    ;; configuration telling us otherwise (see #3250).
    (encode-coding-region (point-min) (point-max) 'utf-8-unix))
  (if (file-remote-p default-directory)
      ;; We lack `process-file-region', so fall back to asynch +
      ;; waiting in remote case.
      (progn
        (magit-start-git (current-buffer) args)
        (while (and magit-this-process
                    (eq (process-status magit-this-process) 'run))
          (sleep-for 0.005)))
    (run-hooks 'magit-pre-call-git-hook)
    (pcase-let* ((process-environment (magit-process-environment))
                 (default-process-coding-system (magit--process-coding-system))
                 (flat-args (magit-process-git-arguments args))
                 (`(,process-buf . ,section)
                  (magit-process-setup magit-git-executable flat-args))
                 (inhibit-read-only t))
      (magit-process-finish
       (apply #'call-process-region (point-min) (point-max)
              magit-git-executable nil process-buf nil flat-args)
       process-buf nil default-directory section))))

(defun magit-run-git-with-logfile (file &rest args)
  "Call Git in a separate process and log its output to FILE.
This function might have a short halflive."
  (apply #'magit-process-file magit-git-executable nil `(:file ,file) nil
         (magit-process-git-arguments args))
  (magit-refresh))

;;; Asynchronous Processes

(defun magit-run-git-async (&rest args)
  "Start Git, prepare for refresh, and return the process object.
ARGS is flattened and then used as arguments to Git.

Display the command line arguments in the echo area.

After Git returns some buffers are refreshed: the buffer that was
current when this function was called (if it is a Magit buffer
and still alive), as well as the respective Magit status buffer.

See `magit-start-process' for more information."
  (message "Running %s %s" magit-git-executable
           (let ((m (mapconcat #'identity (-flatten args) " ")))
             (remove-list-of-text-properties 0 (length m) '(face) m)
             m))
  (magit-start-git nil args))

(defun magit-run-git-with-editor (&rest args)
  "Export GIT_EDITOR and start Git.
Also prepare for refresh and return the process object.
ARGS is flattened and then used as arguments to Git.

Display the command line arguments in the echo area.

After Git returns some buffers are refreshed: the buffer that was
current when this function was called (if it is a Magit buffer
and still alive), as well as the respective Magit status buffer.

See `magit-start-process' and `with-editor' for more information."
  (magit--record-separated-gitdir)
  (magit-with-editor (magit-run-git-async args)))

(defun magit-run-git-sequencer (&rest args)
  "Export GIT_EDITOR and start Git.
Also prepare for refresh and return the process object.
ARGS is flattened and then used as arguments to Git.

Display the command line arguments in the echo area.

After Git returns some buffers are refreshed: the buffer that was
current when this function was called (if it is a Magit buffer
and still alive), as well as the respective Magit status buffer.
If the sequence stops at a commit, make the section representing
that commit the current section by moving `point' there.

See `magit-start-process' and `with-editor' for more information."
  (apply #'magit-run-git-with-editor args)
  (set-process-sentinel magit-this-process #'magit-sequencer-process-sentinel)
  magit-this-process)

(defvar magit-pre-start-git-hook nil)

(defun magit-start-git (input &rest args)
  "Start Git, prepare for refresh, and return the process object.

If INPUT is non-nil, it has to be a buffer or the name of an
existing buffer.  The buffer content becomes the processes
standard input.

Option `magit-git-executable' specifies the Git executable and
option `magit-git-global-arguments' specifies constant arguments.
The remaining arguments ARGS specify arguments to Git, they are
flattened before use.

After Git returns some buffers are refreshed: the buffer that was
current when this function was called (if it is a Magit buffer
and still alive), as well as the respective Magit status buffer.

See `magit-start-process' for more information."
  (run-hooks 'magit-pre-start-git-hook)
  (let ((default-process-coding-system (magit--process-coding-system)))
    (apply #'magit-start-process magit-git-executable input
           (magit-process-git-arguments args))))

(defun magit-start-process (program &optional input &rest args)
  "Start PROGRAM, prepare for refresh, and return the process object.

If optional argument INPUT is non-nil, it has to be a buffer or
the name of an existing buffer.  The buffer content becomes the
processes standard input.

The process is started using `start-file-process' and then setup
to use the sentinel `magit-process-sentinel' and the filter
`magit-process-filter'.  Information required by these functions
is stored in the process object.  When this function returns the
process has not started to run yet so it is possible to override
the sentinel and filter.

After the process returns, `magit-process-sentinel' refreshes the
buffer that was current when `magit-start-process' was called (if
it is a Magit buffer and still alive), as well as the respective
Magit status buffer."
  (pcase-let*
      ((`(,process-buf . ,section)
        (magit-process-setup program args))
       (process
        (let ((process-connection-type
               ;; Don't use a pty, because it would set icrnl
               ;; which would modify the input (issue #20).
               (and (not input) magit-process-connection-type))
              (process-environment (magit-process-environment))
              (default-process-coding-system (magit--process-coding-system)))
          (apply #'start-file-process
                 (file-name-nondirectory program)
                 process-buf program args))))
    (with-editor-set-process-filter process #'magit-process-filter)
    (set-process-sentinel process #'magit-process-sentinel)
    (set-process-buffer   process process-buf)
    (when (eq system-type 'windows-nt)
      ;; On w32, git expects UTF-8 encoded input, ignore any user
      ;; configuration telling us otherwise.
      (set-process-coding-system process 'utf-8-unix))
    (process-put process 'section section)
    (process-put process 'command-buf (current-buffer))
    (process-put process 'default-dir default-directory)
    (when inhibit-magit-refresh
      (process-put process 'inhibit-refresh t))
    (oset section process process)
    (with-current-buffer process-buf
      (set-marker (process-mark process) (point)))
    (when input
      (with-current-buffer input
        (process-send-region process (point-min) (point-max))
        (process-send-eof    process)))
    (setq magit-this-process process)
    (oset section value process)
    (magit-process-display-buffer process)
    process))

(defun magit-parse-git-async (&rest args)
  (setq args (magit-process-git-arguments args))
  (let ((command-buf (current-buffer))
        (process-buf (generate-new-buffer " *temp*"))
        (toplevel (magit-toplevel)))
    (with-current-buffer process-buf
      (setq default-directory toplevel)
      (let ((process
             (let ((process-connection-type nil)
                   (process-environment (magit-process-environment))
                   (default-process-coding-system
                     (magit--process-coding-system)))
               (apply #'start-file-process "git" process-buf
                      magit-git-executable args))))
        (process-put process 'command-buf command-buf)
        (process-put process 'parsed (point))
        (setq magit-this-process process)
        process))))

;;; Process Internals

(defun magit-process-setup (program args)
  (magit-process-set-mode-line program args)
  (let ((pwd default-directory)
        (buf (magit-process-buffer t)))
    (cons buf (with-current-buffer buf
                (prog1 (magit-process-insert-section pwd program args nil nil)
                  (backward-char 1))))))

(defun magit-process-insert-section (pwd program args &optional errcode errlog)
  (let ((inhibit-read-only t)
        (magit-insert-section--parent magit-root-section)
        (magit-insert-section--oldroot nil))
    (goto-char (1- (point-max)))
    (magit-insert-section (process)
      (insert (if errcode
                  (format "%3s " (propertize (number-to-string errcode)
                                             'face 'magit-process-ng))
                "run "))
      (unless (equal (expand-file-name pwd)
                     (expand-file-name default-directory))
        (insert (file-relative-name pwd default-directory) ?\s))
      (cond
       ((and args (equal program magit-git-executable))
        (setq args (-split-at (length magit-git-global-arguments) args))
        (insert (propertize program 'face 'magit-section-heading) " ")
        (insert (propertize (char-to-string magit-ellipsis)
                            'face 'magit-section-heading
                            'help-echo (mapconcat #'identity (car args) " ")))
        (insert " ")
        (insert (propertize (mapconcat #'shell-quote-argument (cadr args) " ")
                            'face 'magit-section-heading)))
       ((and args (equal program shell-file-name))
        (insert (propertize (cadr args) 'face 'magit-section-heading)))
       (t
        (insert (propertize program 'face 'magit-section-heading) " ")
        (insert (propertize (mapconcat #'shell-quote-argument args " ")
                            'face 'magit-section-heading))))
      (magit-insert-heading)
      (when errlog
        (insert-file-contents errlog)
        (goto-char (1- (point-max))))
      (insert "\n"))))

(defun magit-process-truncate-log ()
  (let* ((head nil)
         (tail (oref magit-root-section children))
         (count (length tail)))
    (when (> (1+ count) magit-process-log-max)
      (while (and (cdr tail)
                  (> count (/ magit-process-log-max 2)))
        (let* ((inhibit-read-only t)
               (section (car tail))
               (process (oref section process)))
          (cond ((not process))
                ((memq (process-status process) '(exit signal))
                 (delete-region (oref section start)
                                (1+ (oref section end)))
                 (cl-decf count))
                (t
                 (push section head))))
        (pop tail))
      (oset magit-root-section children
            (nconc (reverse head) tail)))))

(defun magit-process-sentinel (process event)
  "Default sentinel used by `magit-start-process'."
  (when (memq (process-status process) '(exit signal))
    (setq event (substring event 0 -1))
    (when (string-match "^finished" event)
      (message (concat (capitalize (process-name process)) " finished")))
    (magit-process-finish process)
    (when (eq process magit-this-process)
      (setq magit-this-process nil))
    (unless (process-get process 'inhibit-refresh)
      (let ((command-buf (process-get process 'command-buf)))
        (if (buffer-live-p command-buf)
            (with-current-buffer command-buf
              (magit-refresh))
          (with-temp-buffer
            (setq default-directory (process-get process 'default-dir))
            (magit-refresh)))))))

(defun magit-sequencer-process-sentinel (process event)
  "Special sentinel used by `magit-run-git-sequencer'."
  (when (memq (process-status process) '(exit signal))
    (magit-process-sentinel process event)
    (when-let ((process-buf (process-buffer process)))
      (when (buffer-live-p process-buf)
        (when-let ((status-buf (with-current-buffer process-buf
                                 (magit-mode-get-buffer 'magit-status-mode))))
          (with-current-buffer status-buf
            (--when-let
                (magit-get-section
                 `((commit . ,(magit-rev-parse "HEAD"))
                   (,(pcase (car (cadr (-split-at
                                        (1+ (length magit-git-global-arguments))
                                        (process-command process))))
                       ((or "rebase" "am")   'rebase-sequence)
                       ((or "cherry-pick" "revert") 'sequence)))
                   (status)))
              (goto-char (oref it start))
              (magit-section-update-highlight))))))))

(defun magit-process-filter (proc string)
  "Default filter used by `magit-start-process'."
  (with-current-buffer (process-buffer proc)
    (let ((inhibit-read-only t))
      (magit-process-yes-or-no-prompt proc string)
      (magit-process-username-prompt  proc string)
      (magit-process-password-prompt  proc string)
      (goto-char (process-mark proc))
      (setq string (propertize string 'magit-section
                               (process-get proc 'section)))
      ;; Find last ^M in string.  If one was found, ignore
      ;; everything before it and delete the current line.
      (let ((ret-pos (length string)))
        (while (and (>= (cl-decf ret-pos) 0)
                    (/= ?\r (aref string ret-pos))))
        (if (< ret-pos 0)
            (insert string)
          (delete-region (line-beginning-position) (point))
          (insert (substring string (1+ ret-pos)))))
      (set-marker (process-mark proc) (point)))))

(defmacro magit-process-kill-on-abort (proc &rest body)
  (declare (indent 1) (debug (form body)))
  (let ((map (cl-gensym)))
    `(let ((,map (make-sparse-keymap)))
       (set-keymap-parent ,map minibuffer-local-map)
       (define-key ,map "\C-g"
         (lambda ()
           (interactive)
           (ignore-errors (kill-process ,proc))
           (abort-recursive-edit)))
       (let ((minibuffer-local-map ,map))
         ,@body))))

(defun magit-process-yes-or-no-prompt (process string)
  "Forward Yes-or-No prompts to the user."
  (when-let ((beg (string-match magit-process-yes-or-no-prompt-regexp string)))
    (let ((max-mini-window-height 30))
      (process-send-string
       process
       (downcase
        (concat
         (match-string
          (if (save-match-data
                (magit-process-kill-on-abort process
                  (yes-or-no-p (substring string 0 beg)))) 1 2)
          string)
         "\n"))))))

(defun magit-process-password-auth-source (key)
  "Use `auth-source-search' to get a password.
If found, return the password.  Otherwise, return nil."
  (require 'auth-source)
  (let ((secret (plist-get (car (auth-source-search :max 1 :host key
                                                    :require '(:host)))
                           :secret)))
    (if (functionp secret)
        (funcall secret)
      secret)))

(defun magit-process-password-prompt (process string)
  "Find a password based on prompt STRING and send it to git.
Use `magit-process-password-prompt-regexps' to find a known
prompt.  If and only if one is found, then call functions in
`magit-process-find-password-functions' until one of them returns
the password.  If all function return nil, then read the password
from the user."
  (--when-let (magit-process-match-prompt
               magit-process-password-prompt-regexps string)
    (process-send-string
     process (magit-process-kill-on-abort process
               (concat (or (--when-let (match-string 99 string)
                             (run-hook-with-args-until-success
                              'magit-process-find-password-functions it))
                           (read-passwd it))
                       "\n")))))

(defun magit-process-username-prompt (process string)
  "Forward username prompts to the user."
  (--when-let (magit-process-match-prompt
               magit-process-username-prompt-regexps string)
    (process-send-string
     process (magit-process-kill-on-abort process
               (concat (read-string it nil nil (user-login-name)) "\n")))))

(defun magit-process-match-prompt (prompts string)
  "Match STRING against PROMPTS and set match data.
Return the matched string suffixed with \": \", if needed."
  (when (--any-p (string-match it string) prompts)
    (let ((prompt (match-string 0 string)))
      (cond ((string-suffix-p ": " prompt) prompt)
            ((string-suffix-p ":"  prompt) (concat prompt " "))
            (t                             (concat prompt ": "))))))

(defun magit--process-coding-system ()
  (let ((fro (or magit-git-output-coding-system
                 (car default-process-coding-system)))
        (to (cdr default-process-coding-system)))
    (if magit-process-ensure-unix-line-ending
        (cons (coding-system-change-eol-conversion fro 'unix)
              (coding-system-change-eol-conversion to 'unix))
      (cons fro to))))

(defvar magit-credential-hook nil
  "Hook run before Git needs credentials.")

(defvar magit-credential-cache-daemon-process nil)

(defun magit-maybe-start-credential-cache-daemon ()
  "Maybe start a `git-credential-cache--daemon' process.

If such a process is already running or if the value of option
`magit-credential-cache-daemon-socket' is nil, then do nothing.
Otherwise start the process passing the value of that options
as argument."
  (unless (or (not magit-credential-cache-daemon-socket)
              (process-live-p magit-credential-cache-daemon-process)
              (memq magit-credential-cache-daemon-process
                    (list-system-processes)))
    (setq magit-credential-cache-daemon-process
          (or (--first (let* ((attr (process-attributes it))
                              (comm (cdr (assq 'comm attr)))
                              (user (cdr (assq 'user attr))))
                         (and (string= comm "git-credential-cache--daemon")
                              (string= user user-login-name)))
                       (list-system-processes))
              (condition-case nil
                  (start-process "git-credential-cache--daemon"
                                 " *git-credential-cache--daemon*"
                                 magit-git-executable
                                 "credential-cache--daemon"
                                 magit-credential-cache-daemon-socket)
                ;; Some Git implementations (e.g. Windows) won't have
                ;; this program; if we fail the first time, stop trying.
                ((debug error)
                 (remove-hook 'magit-credential-hook
                              #'magit-maybe-start-credential-cache-daemon)))))))

(add-hook 'magit-credential-hook #'magit-maybe-start-credential-cache-daemon)

(defun tramp-sh-handle-start-file-process--magit-tramp-process-environment
    (fn name buffer program &rest args)
  (if magit-tramp-process-environment
      (apply fn name buffer
             (car magit-tramp-process-environment)
             (append (cdr magit-tramp-process-environment)
                     (cons program args)))
    (apply fn name buffer program args)))

(advice-add 'tramp-sh-handle-start-file-process :around
            'tramp-sh-handle-start-file-process--magit-tramp-process-environment)

(defun tramp-sh-handle-process-file--magit-tramp-process-environment
    (fn program &optional infile destination display &rest args)
  (if magit-tramp-process-environment
      (apply fn "env" infile destination display
             (append magit-tramp-process-environment
                     (cons program args)))
    (apply fn program infile destination display args)))

(advice-add 'tramp-sh-handle-process-file :around
            'tramp-sh-handle-process-file--magit-tramp-process-environment)

(defvar magit-mode-line-process-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<mode-line> <mouse-1>")
      'magit-process-buffer)
    map)
  "Keymap for `mode-line-process'.")

(defun magit-process-set-mode-line (program args)
  "Display the git command (sans arguments) in the mode line."
  (when (equal program magit-git-executable)
    (setq args (nthcdr (length magit-git-global-arguments) args)))
  (let ((str (concat " " (propertize
                          (concat program (and args (concat " " (car args))))
                          'mouse-face 'highlight
                          'keymap magit-mode-line-process-map
                          'help-echo "mouse-1: Show process buffer"
                          'face 'magit-mode-line-process))))
    (magit-repository-local-set 'mode-line-process str)
    (dolist (buf (magit-mode-get-buffers))
      (with-current-buffer buf
        (setq mode-line-process str)))
    (force-mode-line-update t)))

(defun magit-process-set-mode-line-error-status (&optional error str)
  "Apply an error face to the string set by `magit-process-set-mode-line'.

If ERROR is supplied, include it in the `mode-line-process' tooltip.

If STR is supplied, it replaces the `mode-line-process' text."
  (setq str (or str (magit-repository-local-get 'mode-line-process)))
  (when str
    (setq error (format "%smouse-1: Show process buffer"
                        (if (stringp error)
                            (concat error "\n\n")
                          "")))
    (setq str (concat " " (propertize
                           (substring-no-properties str 1)
                           'mouse-face 'highlight
                           'keymap magit-mode-line-process-map
                           'help-echo error
                           'face 'magit-mode-line-process-error)))
    (magit-repository-local-set 'mode-line-process str)
    (dolist (buf (magit-mode-get-buffers))
      (with-current-buffer buf
        (setq mode-line-process str)))
    (force-mode-line-update t)
    ;; We remove any error status from the mode line when a magit
    ;; buffer is refreshed (see `magit-refresh-buffer'), but we must
    ;; ensure that we ignore any refreshes during the remainder of the
    ;; current command -- otherwise a newly-set error status would be
    ;; removed before it was seen.  We set a flag which prevents the
    ;; status from being removed prior to the next command, so that
    ;; the error status is guaranteed to remain visible until then.
    (let ((repokey (magit-repository-local-repository)))
      ;; The following closure captures the repokey value, and is
      ;; added to `pre-command-hook'.
      (cl-labels ((enable-magit-process-unset-mode-line
                   () ;; Remove ourself from the hook variable, so
                      ;; that we only run once.
                   (remove-hook 'pre-command-hook
                                #'enable-magit-process-unset-mode-line)
                   ;; Clear the inhibit flag for the repository in
                   ;; which we set it.
                   (magit-repository-local-set
                    'inhibit-magit-process-unset-mode-line nil repokey)))
        ;; Set the inhibit flag until the next command is invoked.
        (magit-repository-local-set
         'inhibit-magit-process-unset-mode-line t repokey)
        (add-hook 'pre-command-hook
                  #'enable-magit-process-unset-mode-line)))))

(defun magit-process-unset-mode-line-error-status ()
  "Remove any current error status from the mode line."
  (let ((status (or mode-line-process
                    (magit-repository-local-get 'mode-line-process))))
    (when (and status
               (eq (get-text-property 1 'face status)
                   'magit-mode-line-process-error))
      (magit-process-unset-mode-line))))

(defun magit-process-unset-mode-line ()
  "Remove the git command from the mode line."
  (unless (magit-repository-local-get 'inhibit-magit-process-unset-mode-line)
    (magit-repository-local-set 'mode-line-process nil)
    (dolist (buf (magit-mode-get-buffers))
      (with-current-buffer buf (setq mode-line-process nil)))
    (force-mode-line-update t)))

(defvar magit-process-error-message-regexps
  (list "^\\*ERROR\\*: Canceled by user$"
        "^\\(?:error\\|fatal\\|git\\): \\(.*\\)$"
        "^\\(Cannot rebase:.*\\)$"))

(define-error 'magit-git-error "Git error")

(defun magit-process-error-summary (process-buf section)
  "A one-line error summary from the given SECTION."
  (or (and (buffer-live-p process-buf)
           (with-current-buffer process-buf
             (and (oref section content)
                  (save-excursion
                    (goto-char (oref section end))
                    (run-hook-wrapped
                     'magit-process-error-message-regexps
                     (lambda (re)
                       (save-excursion
                         (and (re-search-backward
                               re (oref section start) t)
                              (or (match-string-no-properties 1)
                                  (and (not magit-process-raise-error)
                                       'suppressed))))))))))
      "Git failed"))

(defun magit-process-error-tooltip (process-buf section)
  "Returns the text from SECTION of the PROCESS-BUF buffer.

Limited by `magit-process-error-tooltip-max-lines'."
  (and (integerp magit-process-error-tooltip-max-lines)
       (> magit-process-error-tooltip-max-lines 0)
       (buffer-live-p process-buf)
       (with-current-buffer process-buf
         (save-excursion
           (goto-char (or (oref section content)
                          (oref section start)))
           (buffer-substring-no-properties
            (point)
            (save-excursion
              (forward-line magit-process-error-tooltip-max-lines)
              (goto-char
               (if (> (point) (oref section end))
                   (oref section end)
                 (point)))
              ;; Remove any trailing whitespace.
              (when (re-search-backward "[^[:space:]\n]"
                                        (oref section start) t)
                (forward-char 1))
              (point)))))))

(defvar-local magit-this-error nil)

(defvar magit-process-finish-apply-ansi-colors nil)

(defun magit-process-finish (arg &optional process-buf command-buf
                                 default-dir section)
  (unless (integerp arg)
    (setq process-buf (process-buffer arg))
    (setq command-buf (process-get arg 'command-buf))
    (setq default-dir (process-get arg 'default-dir))
    (setq section     (process-get arg 'section))
    (setq arg         (process-exit-status arg)))
  (when (fboundp 'dired-uncache)
    (dired-uncache default-dir))
  (when (buffer-live-p process-buf)
    (with-current-buffer process-buf
      (let ((inhibit-read-only t)
            (marker (oref section start)))
        (goto-char marker)
        (save-excursion
          (delete-char 3)
          (set-marker-insertion-type marker nil)
          (insert (propertize (format "%3s" arg)
                              'magit-section section
                              'face (if (= arg 0)
                                        'magit-process-ok
                                      'magit-process-ng)))
          (set-marker-insertion-type marker t))
        (when magit-process-finish-apply-ansi-colors
          (ansi-color-apply-on-region (oref section content)
                                      (oref section end)))
        (if (= (oref section end)
               (+ (line-end-position) 2))
            (save-excursion
              (goto-char (1+ (line-end-position)))
              (delete-char -1)
              (oset section content nil))
          (let ((buf (magit-process-buffer t)))
            (when (and (= arg 0)
                       (not (--any-p (eq (window-buffer it) buf)
                                     (window-list))))
              (magit-section-hide section)))))))
  (if (= arg 0)
      ;; Unset the `mode-line-process' value upon success.
      (magit-process-unset-mode-line)
    ;; Otherwise process the error.
    (let ((msg (magit-process-error-summary process-buf section)))
      ;; Change `mode-line-process' to an error face upon failure.
      (if magit-process-display-mode-line-error
          (magit-process-set-mode-line-error-status
           (or (magit-process-error-tooltip process-buf section)
               msg))
        (magit-process-unset-mode-line))
      ;; Either signal the error, or else display the error summary in
      ;; the status buffer and with a message in the echo area.
      (cond
       (magit-process-raise-error
        (signal 'magit-git-error (list (format "%s (in %s)" msg default-dir))))
       ((not (eq msg 'suppressed))
        (when (buffer-live-p process-buf)
          (with-current-buffer process-buf
            (when-let ((status-buf (magit-mode-get-buffer 'magit-status-mode)))
              (with-current-buffer status-buf
                (setq magit-this-error msg)))))
        (message "%s ... [%s buffer %s for details]" msg
                 (if-let ((key (and (buffer-live-p command-buf)
                                    (with-current-buffer command-buf
                                      (car (where-is-internal
                                            'magit-process-buffer))))))
                     (format "Hit %s to see" (key-description key))
                   "See")
                 (buffer-name process-buf))))))
  arg)

(defun magit-process-display-buffer (process)
  (when (process-live-p process)
    (let ((buf (process-buffer process)))
      (cond ((not (buffer-live-p buf)))
            ((= magit-process-popup-time 0)
             (if (minibufferp)
                 (switch-to-buffer-other-window buf)
               (pop-to-buffer buf)))
            ((> magit-process-popup-time 0)
             (run-with-timer magit-process-popup-time nil
                             (lambda (p)
                               (when (eq (process-status p) 'run)
                                 (let ((buf (process-buffer p)))
                                   (when (buffer-live-p buf)
                                     (if (minibufferp)
                                         (switch-to-buffer-other-window buf)
                                       (pop-to-buffer buf))))))
                             process))))))

(defun magit--log-action (summary line list)
  (let (heading lines)
    (if (cdr list)
        (progn (setq heading (funcall summary list))
               (setq lines (mapcar line list)))
      (setq heading (funcall line (car list))))
    (with-current-buffer (magit-process-buffer t)
      (goto-char (1- (point-max)))
      (let ((inhibit-read-only t))
        (magit-insert-section (message)
          (magit-insert-heading (concat "  * " heading))
          (when lines
            (dolist (line lines)
              (insert line "\n"))
            (insert "\n"))))
      (let ((inhibit-message t))
        (when heading
          (setq lines (cons heading lines)))
        (message (mapconcat #'identity lines "\n"))))))

(provide 'magit-process)
;;; magit-process.el ends here
