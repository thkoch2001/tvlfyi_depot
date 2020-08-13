;;; ~/.doom.d/org-config.el -*- lexical-binding: t; -*-
;;;

(defun notes-file (f)
  (concat org-directory (if (string-prefix-p "/" f) "" "/") f))

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
 org-ellipsis "⤵"
 org-imenu-depth 9
 org-capture-templates
 `(("t" "Todo" entry
    (file +org-default-todo-file)
    "* TODO %?\n%i"
    :kill-buffer t)

   ("m" "Email" entry
    (file +org-default-todo-file)
    "* TODO [%l[%:subject]] :email:\n%i")

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

   ("d" "Data recording")
   )

 org-capture-templates-contexts
 `(("px" ((in-file . "/home/griffin/code/xanthous/.*"))))

 org-deadline-warning-days 1
 org-agenda-skip-scheduled-if-deadline-is-shown 'todo
 org-todo-keywords '((sequence "TODO(t)" "ACTIVE(a)" "|" "DONE(d)" "RUNNING(r)")
                     (sequence "NEXT(n)" "WAITING(w)" "LATER(l)" "|" "CANCELLED(c)"))
 org-agenda-custom-commands
 '(("p" "Sprint Tasks" tags-todo "sprint")
   ("i" "Inbox" tags "inbox")
   ("r" "Running jobs" todo "RUNNING")
   ("w" "@Work" tags-todo "@work")
   ("n" . "Next...")
   ("np" "Next Sprint" tags-todo "next_sprint|sprint_planning"))

 org-agenda-dim-blocked-tasks nil
 org-enforce-todo-dependencies nil

 org-babel-clojure-backend 'cider)

(defun +grfn/insert-work-template ()
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

(defun +grfn/insert-org-template ()
  (interactive)
  (pcase (buffer-file-name)
    ((s-contains "/work/") (+grfn/insert-work-template))))

;;; TODO: this doesn't work?
(define-auto-insert "\\.org?$" #'grfn/insert-org-template t)
