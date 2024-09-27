(require 'chart)
(require 'dash)
(require 'map)

(require 'gio-list-apps) ;; native module!

(defun goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting for the line number input"
  (interactive)
  (unwind-protect
      (progn
        (setq-local display-line-numbers t)
        (let ((target (read-number "Goto line: ")))
          (avy-push-mark)
          (goto-line target)))
    (setq-local display-line-numbers nil)))

(defun esk-add-watchwords ()
  (font-lock-add-keywords
   nil '(("\\<\\(FIX\\(ME\\)?\\|TODO\\|DEBUG\\|HACK\\|REFACTOR\\|NOCOMMIT\\)"
          1 font-lock-warning-face t))))

(add-hook 'prog-mode-hook 'esk-add-watchwords)

(defun esk-sudo-edit (&optional arg)
  (interactive "p")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:" (read-file-name "File: ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

;; Get the nix store path for a given derivation.
;; If the derivation has not been built before, this will trigger a build.
(defun nix-store-path (derivation)
  (let ((expr (concat "with import <nixos> {}; " derivation)))
    (s-chomp (shell-command-to-string (concat "nix-build -E '" expr "'")))))

(defun insert-nix-store-path ()
  (interactive)
  (let ((derivation (read-string "Derivation name (in <nixos>): ")))
    (insert (nix-store-path derivation))))

(defun toggle-force-newline ()
  "Buffer-local toggle for enforcing final newline on save."
  (interactive)
  (setq-local require-final-newline (not require-final-newline))
  (message "require-final-newline in buffer %s is now %s"
           (buffer-name)
           require-final-newline))

(defun list-external-commands ()
  "Creates a list of all external commands available on $PATH
  while filtering NixOS wrappers."
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

(defvar external-command-flag-overrides
  '(("google-chrome" . "--force-device-scale-factor=1.4"))

  "This setting lets me add additional flags to specific commands
  that are run interactively via `run-external-command'.")

(defun run-external-command--handler (cmd)
  "Execute the specified command and notify the user when it
  finishes."
    (let* ((extra-flags (cdr (assoc cmd external-command-flag-overrides)))
           (cmd (if extra-flags (s-join " " (list cmd extra-flags)) cmd)))
      (message "Starting %s..." cmd)
      (set-process-sentinel
       (start-process-shell-command cmd nil cmd)
       (lambda (process event)
         (when (string= event "finished\n")
           (message "%s process finished." process))))))

(defun run-external-command ()
  "Prompts the user with a list of all installed applications and
  lets them select one to launch."

  (interactive)
  (let ((external-commands-list (list-external-commands)))
    (run-external-command--handler
     (completing-read "Command: " external-commands-list
                      nil                             ;; predicate
                      t                               ;; require-match
                      nil                             ;; initial-input
                      ;; hist
                      'external-commands-history))))

(defun password-store-lookup (&optional password-store-dir)
  "Interactive password-store lookup function that actually uses
the GPG agent correctly."

  (interactive)

  (let* ((entry (completing-read "Copy password of entry: "
                   (password-store-list (or password-store-dir
                                            (password-store-dir)))
                   nil ;; predicate
                   t   ;; require-match
                   ))
         (password (or (let ((epa-suppress-error-buffer t))
                         (auth-source-pass-get 'secret entry))
                       (error "failed to decrypt '%s', wrong password?" entry))))
    (password-store-clear)
    (kill-new password)
    (setq password-store-kill-ring-pointer kill-ring-yank-pointer)
    (message "Copied %s to the kill ring. Will clear in %s seconds."
             entry (password-store-timeout))
    (setq password-store-timeout-timer
          (run-at-time (password-store-timeout)
                       nil 'password-store-clear))))

(defhydra mc/mark-more-hydra (:color pink)
  ("<up>" mc/mmlte--up "Mark previous like this")
  ("<down>" mc/mmlte--down "Mark next like this")
  ("<left>" mc/mmlte--left (if (eq mc/mark-more-like-this-extended-direction 'up)
                               "Skip past the cursor furthest up"
                             "Remove the cursor furthest down"))
  ("<right>" mc/mmlte--right (if (eq mc/mark-more-like-this-extended-direction 'up)
                                 "Remove the cursor furthest up"
                               "Skip past the cursor furthest down"))
  ("f" nil "Finish selecting"))

;; Mute the message that mc/mmlte wants to print on its own
(advice-add 'mc/mmlte--message :around (lambda (&rest args) (ignore)))

(defun mc/mark-dwim (arg)
  "Select multiple things, but do what I mean."

  (interactive "p")
  (if (not (region-active-p)) (mc/mark-next-lines arg)
    (if (< 1 (count-lines (region-beginning)
                          (region-end)))
        (mc/edit-lines arg)
      ;; The following is almost identical to `mc/mark-more-like-this-extended',
      ;; but uses a hydra (`mc/mark-more-hydra') instead of a transient key map.
      (mc/mmlte--down)
      (mc/mark-more-hydra/body))))

(setq mc/cmds-to-run-for-all '(kill-region paredit-newline))

(setq mc/cmds-to-run-once '(mc/mark-dwim
                            mc/mark-more-hydra/mc/mmlte--down
                            mc/mark-more-hydra/mc/mmlte--left
                            mc/mark-more-hydra/mc/mmlte--right
                            mc/mark-more-hydra/mc/mmlte--up
                            mc/mark-more-hydra/mmlte--up
                            mc/mark-more-hydra/nil))

(defun insert-todo-comment (prefix todo)
  "Insert a comment at point with something for me to do."

  (interactive "P\nsWhat needs doing? ")
  (save-excursion
    (move-end-of-line nil)
    (insert (format " %s TODO(%s): %s"
                    (s-trim-right comment-start)
                    (if prefix (read-string "Who needs to do this? ")
                      (getenv "USER"))
                    todo))))

;; Custom text scale adjustment functions that operate on the entire instance
(defun modify-text-scale (factor)
  (set-face-attribute 'default nil
                      :height (+ (* factor 5) (face-attribute 'default :height))))

(defun increase-default-text-scale (prefix)
  "Increase default text scale in all Emacs frames, or just the
  current frame if PREFIX is set."

  (interactive "P")
  (if prefix (text-scale-increase 1)
    (modify-text-scale 1)))

(defun decrease-default-text-scale (prefix)
  "Increase default text scale in all Emacs frames, or just the
  current frame if PREFIX is set."

  (interactive "P")
  (if prefix (text-scale-decrease 1)
    (modify-text-scale -1)))

(defun set-default-text-scale (prefix &optional to)
  "Set the default text scale to the specified value, or the
  default. Restores current frame's text scale only, if PREFIX is
  set."

  (interactive "P")
  (if prefix (text-scale-adjust 0)
    (set-face-attribute 'default nil :height (or to 120))))

(defun screenshot-select (filename)
  "Take a screenshot based on a mouse-selection and save it to
  ~/screenshots."
  (interactive "sScreenshot filename: ")
  (let* ((path (f-join "~/screenshots"
                       (format "%s-%d.png"
                               (if (string-empty-p filename) "shot" filename)
                               (time-convert nil 'integer)))))
    (shell-command (format "maim --select %s" path))
    (message "Wrote screenshot to %s" path)))

(defun graph-unread-mails ()
  "Create a bar chart of unread mails based on notmuch tags.
  Certain tags are excluded from the overview."

  (interactive)
  (let ((tag-counts
         (-keep (-lambda ((name . search))
                  (let ((count
                         (string-to-number
                          (s-trim
                           (notmuch-command-to-string "count" search "and" "tag:unread")))))
                    (when (>= count 1) (cons name count))))
                (notmuch-hello-generate-tag-alist '("unread" "signed" "attachment" "important")))))

    (chart-bar-quickie
     (if (< (length tag-counts) 6)
         'vertical 'horizontal)
     "Unread emails"
     (-map #'car tag-counts) "Tag:"
     (-map #'cdr tag-counts) "Count:")))

(defun notmuch-show-open-or-close-subthread (&optional prefix)
  "Open or close the subthread from (and including) the message at point."
  (interactive "P")
  (save-excursion
    (let ((current-depth (map-elt (notmuch-show-get-message-properties) :depth 0)))
      (loop do (notmuch-show-message-visible (notmuch-show-get-message-properties) prefix)
            until (or (not (notmuch-show-goto-message-next))
                      (= (map-elt (notmuch-show-get-message-properties) :depth) current-depth)))))
  (force-window-update))

(defun vterm-send-ctrl-x ()
  "Sends `C-x' to the libvterm."
  (interactive)
  (vterm-send-key "x" nil nil t))

(defun find-depot-project (dir)
  "Function used in the `project-find-functions' hook list to
  determine the current project root of a depot project."
  (when (s-starts-with? "/depot" dir)
    (if (f-exists-p (f-join dir "default.nix"))
        (cons 'transient dir)
      (find-depot-project (f-parent dir)))))

(add-to-list 'project-find-functions #'find-depot-project)

(defun find-cargo-project (dir)
  "Attempt to find the current project in `project-find-functions'
by looking for a `Cargo.toml' file."
  (when dir
    (unless (equal "/" dir)
      (if (f-exists-p (f-join dir "Cargo.toml"))
          (cons 'transient dir)
        (find-cargo-project (f-parent dir))))))

(add-to-list 'project-find-functions #'find-cargo-project)

(defun magit-find-file-worktree ()
  (interactive)
  "Find a file in the current (ma)git worktree."
  (magit-find-file--internal "{worktree}"
                             (magit-read-file-from-rev "HEAD" "Find file")
                             #'pop-to-buffer-same-window))

(defun zoxide-open-project ()
  "Query Zoxide for paths, and open the result as appropriate (magit or dired)."
  (interactive)
  (zoxide-open-with
   nil
   (lambda (path)
     (condition-case err (magit-status-setup-buffer path)
       (magit-outside-git-repo (dired path))))))

(defun toggle-nix-test-and-exp ()
  "Switch between the .nix and .exp file in a Tvix/Nix test."
  (interactive)
  (let* ((file (buffer-file-name))
         (other (if (s-suffix? ".nix" file)
                    (s-replace-regexp ".nix$" ".exp" file)
                  (if (s-suffix? ".exp" file)
                      (s-replace-regexp ".exp$" ".nix" file)
                    (error "Not a .nix/.exp file!")))))
    (find-file other)))

(defun reliably-switch-buffer ()
  "Reliably and interactively switch buffers, without ending up in a
situation where the buffer was renamed during selection and an
empty new buffer is created.

This is done by, in contrast to most buffer-switching functions,
retaining a list of the buffer *objects* and their associated
names, instead of only their names (which might change)."

  (interactive)
  (let* ((buffers (seq-map (lambda (b) (cons (buffer-name b) b))
                           (seq-filter (lambda (b) (not (string-prefix-p " " (buffer-name b))))
                                       (buffer-list))))

         ;; Annotate buffers that display remote files. I frequently
         ;; want to see it, because I might have identically named
         ;; files open locally and remotely at the same time, and it
         ;; helps with differentiating them.
         (completion-extra-properties
          '(:annotation-function
            (lambda (name)
              (if-let* ((file (buffer-file-name (cdr (assoc name buffers))))
                        (remote (file-remote-p file)))
                  (format " [%s]" remote)))))

         (name (completing-read "Switch to buffer: " (seq-map #'car buffers)))
         (selected (or (cdr (assoc name buffers))
                       ;; Allow users to manually select invisible buffers ...
                       (get-buffer name))))
    (switch-to-buffer (or selected name) nil 't)))

(defun run-xdg-app ()
  "Use `//users/tazjin/gio-list-apps' to retrieve a list of
installed (and visible) XDG apps, and let users launch them."
  (interactive)
  (let* ((apps (taz-list-xdg-apps))

         ;; Display the command that will be run as an annotation
         (completion-extra-properties
          '(:annotation-function (lambda (app) (format " [%s]" (cdr (assoc app apps)))))))

    (run-external-command--handler (cdr (assoc (completing-read "App: " apps nil t) apps)))))

(defun advice-remove-all (sym)
  "Remove all advices from symbol SYM."
  (interactive "aFunction symbol: ")
  (advice-mapc (lambda (advice _props) (advice-remove sym advice)) sym))

(defun M-x-always-same-window ()
  "Run `execute-extended-command', but ensure that whatever it does
always opens in the same window in which the command was invoked."
  (interactive)
  (let ((display-buffer-overriding-action
         '((display-buffer-same-window) . ((inhibit-same-window . nil)))))
    (call-interactively #'execute-extended-command)))

;; Some Niri hackery

(defun niri-go-anywhere ()
  "Interactively select and switch to an open Niri window, or an
  Emacs buffer."
  (interactive)
  (let* ((windows
          (json-parse-string
           (shell-command-to-string "niri msg -j windows")))
         (selectable (seq-map (lambda (w) (cons (format "%s [%s]"
                                                        (map-elt w "title")
                                                        (map-elt w "app_id"))
                                                w))
                              windows))
         (target (completing-read "Switch to window: " (seq-map #'car selectable)))
         (target-window (cdr (assoc target selectable))))
    (shell-command (format "niri msg action focus-window --id %d"
                           (map-elt target-window "id")))))


(provide 'functions)
