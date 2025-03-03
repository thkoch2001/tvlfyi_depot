;;; -*- lexical-binding: t; -*-

(defun +aspen/org-setup ()
  (setq-local truncate-lines -1)
  (display-line-numbers-mode -1)
  (line-number-mode -1))

(add-hook 'org-mode-hook #'+aspen/org-setup)

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
  (loop for tag in project-tags
        collect `(,(aspen/org-project-tag->keys tag)
                  ,(aspen/org-project-tag->name tag)
                  tags-todo
                  ,tag)))

(defun aspen/org-projects ()
  (loop for (tag) in
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

   ("p" . "Project...")
   ,@(aspen/org-projects->agenda-commands (aspen/org-projects)))

 org-agenda-dim-blocked-tasks nil
 org-enforce-todo-dependencies nil

 org-babel-clojure-backend 'cider)

(defun +aspen/insert-work-template ()
  (interactive)
  (goto-char (point-min))
  (forward-line)
  (insert "#+TODO: TODO(t) NEXT(n) ACTIVE(a) | DONE(d) PR(p) RUNNING(r) TESTING(D)
#+TODO: BLOCKED(b) BACKLOG(l) PROPOSED(o) | CANCELLED(c)
#+FILETAGS: @work
#+FILETAGS: @work
#+PROPERTY: Effort_ALL 0 4:00 8:00 12:00 20:00 32:00
#+PROPERTY: ESTIMATE_ALL 0 1 2 3 5 8
#+PROPERTY: STORY-TYPE_ALL Feature Bug Chore
#+PROPERTY: NOBLOCKING t
#+COLUMNS: %TODO %40ITEM(Task) %17EFFORT(Estimated){:} %CLOCKSUM(Time Spent) %17STORY-TYPE(Type) %TAGS"))

(defun +aspen/insert-org-template ()
  (interactive)
  (pcase (buffer-file-name)
    ((s-contains "/work/") (+aspen/insert-work-template))))

;;; TODO: this doesn't work?
(define-auto-insert "\\.org?$" #'aspen/insert-org-template t)

(defun forge--post-submit-around---link-pr-to-org-item
    (orig)
  (let ((cb (funcall orig)))
    (lambda (value headers status req)
      (prog1 (funcall cb value headers status req)
        (aspen/at-org-clocked-in-item
         (let ((url (alist-get 'html_url value))
               (number (alist-get 'number value)))
           (org-set-property
            "pull-request"
            (org-make-link-string
             url
             (format "%s/%s/%d"
                     (->> value
                          (alist-get 'base)
                          (alist-get 'repo)
                          (alist-get 'name))
                     (->> value
                          (alist-get 'base)
                          (alist-get 'repo)
                          (alist-get 'owner)
                          (alist-get 'login))
                     number)))))))))

(advice-add
 #'forge--post-submit-callback
 :around #'forge--post-submit-around---link-pr-to-org-item)

(set-face-foreground 'org-block +solarized-s-base00)
(setq whitespace-global-modes '(not org-mode magit-mode vterm-mode))
(setf (alist-get 'file org-link-frame-setup) 'find-file-other-window)
(set-face-foreground 'org-block +solarized-s-base00)

;; (add-hook! org-mode
;;   (set-company-backend! 'org-mode
;;     '(:separate company-ob-postgresql
;;                 company-dabbrev
;;                 company-yasnippet
;;                 company-ispell)))
