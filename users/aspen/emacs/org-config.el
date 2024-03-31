;;; org-config.el -*- lexical-binding: t; -*-

(defun +aspen/org-setup ()
  (setq-local truncate-lines -1)
  (display-line-numbers-mode -1)
  (line-number-mode -1)
  (when-let*
      ((path (buffer-file-name))
       (fn (file-name-nondirectory path))
       (equal (string-equal fn "config.org")))
    (paxedit-mode 1)
    (display-line-numbers-mode 1)
    (flyspell-mode -1)
    (org-config-mode 1)))

(add-hook 'org-mode-hook #'+aspen/org-setup 50)

(defun notes-file (f)
  (concat org-directory (if (string-prefix-p "/" f) "" "/") f))

(defun aspen/org-project-tag->key (tag)
  (s-replace-regexp "^project__" "" tag))

(defun aspen/org-project-tag->name (tag)
  (s-titleized-words
   (s-join " " (s-split "_" (aspen/org-project-tag->key tag)))))

(defun aspen/org-project-tag->keys (tag)
  (s-join "" (cons "p"
                   (-map (lambda (s) (substring-no-properties s 0 1))
                         (s-split "_" (aspen/org-project-tag->key tag))))))

(defun aspen/org-projects->agenda-commands (project-tags)
  (cl-loop for tag in project-tags
           collect `(,(aspen/org-project-tag->keys tag)
                     ,(aspen/org-project-tag->name tag)
                     tags-todo
                     ,tag)))

(defun aspen/org-projects ()
  (cl-loop for (tag) in
           (org-global-tags-completion-table
            (directory-files-recursively "~/notes" "\\.org$"))
           when (s-starts-with-p "project__" tag)
           collect tag))

(comment
 (aspen/org-projects->agenda-commands (aspen/org-projects))
 )

(setq
 org-directory (expand-file-name "~/notes")
 +org-dir (expand-file-name "~/notes")
 org-default-notes-file (concat org-directory "/inbox.org")
 +org-default-todo-file (concat org-directory "/inbox.org")
 org-agenda-files (directory-files-recursively
                   "~/notes" "\\.org$")
 org-refile-targets '((org-agenda-files :maxlevel . 3))
 org-outline-path-complete-in-steps nil
 org-refile-use-outline-path t
 org-file-apps `((auto-mode . emacs)
                 (,(rx (or (and "." (optional "x") (optional "htm") (optional "l") buffer-end)
                           (and buffer-start "http" (optional "s") "://")))
                  . "firefox %s")
                 (,(rx ".pdf" buffer-end) . "apvlv %s")
                 (,(rx "." (or "png"
                               "jpg"
                               "jpeg"
                               "gif"
                               "tif"
                               "tiff")
                       buffer-end)
                  . "feh %s"))
 org-log-done 'time
 org-archive-location "~/notes/trash::* From %s"
 org-cycle-separator-lines 2
 org-hidden-keywords '(title)
 org-tags-column -130
 org-ellipsis "…"
 org-imenu-depth 9
 org-capture-templates
 `(("t" "Todo" entry
    (file +org-default-todo-file)
    "* TODO %?\n%i"
    :kill-buffer t)

   ("m" "Email" entry
    (file +org-default-todo-file)
    "* TODO [[%L][%:subject]] :email:\n%i")

   ("n" "Notes" entry
    (file +org-default-todo-file)
    "* %U %?\n%i"
    :prepend t
    :kill-buffer t)

   ("c" "Task note" entry
    (clock)
    "* %U %?\n%i[%l[Context]]\n"
    :kill-buffer t
    :unnarrowed t)

   ("p" "Projects")
   ("px" "Xanthous" entry
    (file+headline ,(notes-file "xanthous.org") "Backlog")
    "* TODO %?\nContext %a\nIn task: %K")
   ("pt" "Tvix" entry
    (file+headline ,(notes-file "tvix.org") "Tvix TODO")
    "* TODO %?\nContext %a\nIn task: %K")
   ("pw" "Windtunnel" entry
    (file+headline ,(notes-file "windtunnel.org") "Inbox")
    "* TODO %i%?\nContext: %a\nIn task: %K")
   )

 org-capture-templates-contexts
 `(("px" ((in-file . "/home/aspen/code/depot/users/aspen/xanthous/.*")))
   ("e" ((in-mode . "notmuch-show-mode"))))

 org-deadline-warning-days 1
 org-agenda-skip-scheduled-if-deadline-is-shown 'todo
 org-todo-keywords '((sequence "TODO(t)" "ACTIVE(a)" "|" "DONE(d)" "RUNNING(r)")
                     (sequence "NEXT(n)" "WAITING(w)" "LATER(l)" "|" "CANCELLED(c)"))
 org-agenda-custom-commands
 `(("i" "Inbox" tags "inbox")
   ("r" "Running jobs" todo "RUNNING")
   ("w" "@Work" tags-todo "@work")
   ("n" . "Next...")
   ("nw" "Next @Work" tags-todo "@work&next")
   ("nt" "Next tooling" tags-todo "tooling")

   ;; ("p" . "Project...")
   ;; ,@(aspen/org-projects->agenda-commands (aspen/org-projects))
   )

 org-agenda-dim-blocked-tasks nil
 org-enforce-todo-dependencies nil

 org-babel-clojure-backend 'cider)

(setq whitespace-global-modes '(not org-mode magit-mode vterm-mode))
(setf (alist-get 'file org-link-frame-setup) 'find-file-other-window)
