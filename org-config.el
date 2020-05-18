;;; ~/.doom.d/org-config.el -*- lexical-binding: t; -*-
;;;

(defun notes-file (f)
  (concat org-directory (if (string-prefix-p "/" f) "" "/") f))

(setq
 org-directory (expand-file-name "~/notes")
 +org-dir (expand-file-name "~/notes")
 org-default-notes-file (concat org-directory "/inbox.org")
 +org-default-todo-file (concat org-directory "/inbox.org")
 org-agenda-files (list org-directory)
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

   ("n" "Notes" entry
    (file +org-default-todo-file)
    "* %U %?\n%i"
    :prepend t
    :kill-buffer t)

   ("c" "Task note" entry
    (clock)
    "* %U %?\n%i[[%l][Context]]\n"
    :kill-buffer t
    :unnarrowed t)

   ;; ("d" "Tech debt" entry
   ;;  (file+headline ,(concat org-directory "/work.org")
   ;;                 "Inbox")
   ;;  "* TODO %? :debt:\nContext: %a\nIn task: %K"
   ;;  :prepend t
   ;;  :kill-buffer t)

   ("p" "Projects")
   ("px" "Xanthous" entry
    (file+headline ,(notes-file "xanthous.org") "Backlog")
    "* TODO %?\nContext %a\nIn task: %K")

   ("d" "Data recording")
   ;; ("dr" "Reflux data" table-line
   ;;  (file+olp ,(notes-file "personal.org")
   ;;            "Data" "Reflux")
   ;;  "| %U | %^{reflux|0|1|2|3|4|5} | %^{ate 1hr before bed?|Y|N} | %^{ate spicy food yesterday?|Y|N} |"
   ;;  :unnarrowed t
   ;;  :immediate-finish t
   ;;  )
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

 org-babel-clojure-backend 'cider)
