(defvar wc/helm-git-tracked-staged
  (helm-build-in-buffer-source "Tracked, Staged"
    :candidates (shell-command-to-string "git --no-pager diff --name-only --staged")
    :action 'wc/handle-branch))


(defvar wc/helm-print-default-directory
  (helm-build-in-buffer-source "Tracked, Staged"
    :candidates (lambda () '((default-directory)))
    :action 'wc/handle-branch))


(defvar wc/helm-git-tracked-unstaged
  (helm-build-in-buffer-source "Tracked, Unstaged"
    :candidates (shell-command-to-string "git --no-pager diff --name-only")
    :action 'wc/handle-branch))


(defvar wc/helm-git-untracked-unstaged
  (helm-build-in-buffer-source "Untracked, Unstaged"
    :candidates (shell-command-to-string "git ls-files --others --exclude-standard")
    :action 'wc/handle-branch))


(defun wc/helm-git-altered-files ()
  "View a categorized list of altered files within a project."
  (interactive)
  (helm :sources '(wc/helm-print-default-directory
                   ;; wc/helm-git-tracked-staged
                   ;; wc/helm-git-tracked-unstaged
                   ;; wc/helm-git-untracked-unstaged
                   )
        :buffer "*helm git altered files*"))


(defun wc/helm-git-branches ()
  "Reverse-I search using Helm."
  (interactive)
  (helm :sources (helm-build-in-buffer-source "test1"
                 :data (wc/git-branches)
                 :action 'wc/handle-branch)
      :buffer "*helm git branches*"))


(defun wc/open-terminals ()
  "Lists active terminal buffers."
  (interactive)
  (helm :sources (helm-build-in-buffer-source "test1"
                 :data (wc/list-project-terminals)
                 :action 'switch-to-buffer)
      :buffer "*helm projectile terminals*"))


(defun wc/helm-autojump ()
  "Helm interface to autojump."
  (interactive)
  (helm :sources (helm-build-in-buffer-source "test1"
                 :data (wc/autojump-directories)
                 :action (lambda (path) (wc/exec-cmd (format "cd %s" path))))
      :buffer "*helm git branches*"))


(defun wc/helm-shell-history ()
  "Reverse-I search using Helm."
  (interactive)
  (helm :sources (helm-build-in-buffer-source "test1"
                 :data (wc/shell-history)
                 :action 'wc/exec-cmd)
      :buffer "*helm shell history*"))


(defun wc/helm-ctrl-t-find-files ()
  "Fuzzily searches files within a directory."
  (interactive)
  (helm :sources (helm-build-in-buffer-source "test1"
                 :data (shell-command-to-string "ag --hidden --ignore .git -l -g \"\"")
                 :action 'term-send-raw-string)
      :buffer "*helm CTRL_T find files *"))
