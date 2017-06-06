(defun wc/projectile-shell-pop ()
  "Opens `ansi-term' at the project root according to Projectile."
  (interactive)
  (let* ((project-name (projectile-project-root))
         (default-directory project-name)
         (buffer-name (format "ansi-term <%s>" project-name)))
    (if (get-buffer buffer-name)
        (switch-to-buffer buffer-name)
      (ansi-term "/bin/zsh" buffer-name))))


(defun wc/ansi-term-project-p (input)
  (string-match-p "*ansi-term <[^>]+>*" input))


(defun wc/list-project-terminals ()
  "Returns a list of ansi-term buffers with associated projects."
  (interactive)
  (let ((buffer-names (mapcar 'buffer-name (buffer-list))))
    (remove-if-not #'wc/ansi-term-project-p buffer-names)))


(defun wc/open-terminals ()
  "Lists active terminal buffers."
  (interactive)
  (helm :sources (helm-build-in-buffer-source "test1"
                 :data (wc/list-project-terminals)
                 :action 'switch-to-buffer)
      :buffer "*helm projectile terminals*"))


(defun wc/shell-history ()
  (setq history (shell-command-to-string "history"))
  (split-string history "\n"))


(defun wc/git-branches ()
  (setq branches (shell-command-to-string "git branch -a | tr -d '* ' | sed 's/^remotes\\/origin\\///' | sort | uniq"))
  (split-string branches "\n"))


(defun wc/helm-git-branches ()
  "Reverse-I search using Helm."
  (interactive)
  (helm :sources (helm-build-in-buffer-source "test1"
                 :data (wc/git-branches)
                 :action 'wc/handle-branch)
      :buffer "*helm git branches*"))


(defun wc/autojump-directories ()
  (setq directories (shell-command-to-string "j -s | awk '{ if($2 ~ /^\\// && $1 != \"data:\") print;}' | sort -rn | head -n 100 | awk '{print $2}'"))
  (split-string directories "\n"))


(defun wc/helm-autojump ()
  "Helm interface to autojump."
  (interactive)
  (helm :sources (helm-build-in-buffer-source "test1"
                 :data (wc/autojump-directories)
                 :action (lambda (path) (wc/exec-cmd (format "cd %s" path))))
      :buffer "*helm git branches*"))


(defun wc/handle-branch (branch)
  (setq action "git diff")
  (term-send-raw-string (format "%s %s" action branch)))


(defun wc/helm-shell-history ()
  "Reverse-I search using Helm."
  (interactive)
  (helm :sources (helm-build-in-buffer-source "test1"
                 :data (wc/shell-history)
                 :action 'wc/exec-cmd)
      :buffer "*helm shell history*"))


(defun wc/exec-cmd (cmd)
  (term-send-raw-string (format "%s\n" cmd)))


(defun wc/join-erc ()
  "Boots `erc' and autojoins channels."
  (interactive)
  (erc :server "irc.freenode.net" :port "6667" :nick "wpcarro"))


(defun wc/expose-global-binding-in-term (binding)
   (define-key term-raw-map binding
     (lookup-key (current-global-map) binding)))


(defun wc/focus-term-at-bottom ()
  "Moves term to the bottom of the page on insert mode."
  (interactive)
  (end-of-buffer)
  (evil-insert-state)
  (term-send-raw-string "\b"))


(defun wc/bootstrap-ansi-term ()
  "Custom `ansi-term' configuration."
  (interactive)
  (goto-address-mode t)
  (linum-mode nil)
  (local-set-key (kbd "C-h") 'evil-window-left)
  (local-set-key (kbd "C-l") 'evil-window-right)
  (local-set-key (kbd "C-k") 'evil-window-up)
  (local-set-key (kbd "C-j") 'evil-window-down)
  (wc/expose-global-binding-in-term (kbd "M-x"))
  (evil-define-key 'normal term-raw-map
    (kbd "C-c") 'term-interrupt-subjob
    (kbd "i") 'wc/focus-term-at-bottom)
  (define-key term-raw-map (kbd "C-r") 'wc/helm-shell-history)
  (define-key term-raw-map (kbd "M-:") 'eval-expression)
  (define-key term-raw-map (kbd "M-j") 'wc/helm-autojump)
  (define-key term-raw-map (kbd "s-v") 'term-paste))


(defun wc/ansi-term-paste (&optional string)
  "Paste into `ansi-term'."
  (interactive)
  (process-send-string
   (get-buffer-process (current-buffer))
   (if string string (current-kill 0))))


(defun evil-window-vsplit-right ()
  "Vertically split a window and move right."
  (interactive)
  (evil-window-vsplit nil)
  (evil-window-right 1))


(defun evil-window-split-down ()
  "Split a window and move right."
  (interactive)
  (evil-window-split nil)
  (evil-window-down 1))


(defun wc/switch-to-mru-buffer ()
  "Switches to the most recently used buffer, including visible buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) t (selected-frame))))


(defun *-popwin-help-mode-off ()
  "Turn `popwin-mode' off for *Help* buffers."
  (when (boundp 'popwin:special-display-config)
    (customize-set-variable 'popwin:special-display-config
                            (delq 'help-mode popwin:special-display-config))))


(defun *-popwin-help-mode-on ()
  "Turn `popwin-mode' on for *Help* buffers."
  (when (boundp 'popwin:special-display-config)
    (customize-set-variable 'popwin:special-display-config
                            (add-to-list 'popwin:special-display-config 'help-mode nil #'eq))))


(defun wc/custom-erlang-mode-hook ()
  "Jump to and from Elixir, Erlang, Elixir files."
  (define-key erlang-mode-map (kbd "M-,") 'alchemist-goto-jump-back))


(defun helm-ag-neotree-node ()
  "Run Helm-ag on Neotree directory."
  (interactive)
  (let ((search-root (neo-buffer--get-filename-current-line)))
    (if search-root
        ;; search directory
        (progn
          (evil-window-right 1)
          (helm-ag search-root))
      (message "Could not find directory at point."))))


(defun neotree-toggle-project-dir ()
  "Toggle neotree sidebar."
  (interactive)
  (let ((project-dir (projectile-project-root))
        (file-name (buffer-file-name)))
    (neotree-toggle)
    (if project-dir
        (if (neo-global--window-exists-p)
            (progn
              (neotree-dir project-dir)
              (neotree-show)
              (evil-window-mru)))
      (message "Could not find git project root."))))


(defun neotree-reveal-current-buffer ()
  "Reveal current buffer in Neotree."
  (interactive)
  (let ((project-dir (projectile-project-root))
        (file-name (buffer-file-name)))
    (neotree-show)
    (if project-dir
        (if (neo-global--window-exists-p)
            (progn
              (neotree-dir project-dir)
              (neotree-find file-name)
              (evil-window-mru)))
      (message "Could not find git project root."))))


(defun save-buffer-always ()
  "Save the buffer even if it is not modified."
  (interactive)
  (set-buffer-modified-p t)
  (save-buffer))
(global-set-key (kbd "C-x C-s") nil)
(global-set-key (kbd "C-x C-s") 'save-buffer-always)


;; Upgrade all packages
(defun package-upgrade-all ()
  "Upgrade all packages automatically without showing *Packages* buffer."
  (interactive)
  (package-refresh-contents)
  (let (upgrades)
    (cl-flet ((get-version (name where)
                           (let ((pkg (cadr (assq name where))))
                             (when pkg
                               (package-desc-version pkg)))))
      (dolist (package (mapcar #'car package-alist))
        (let ((in-archive (get-version package package-archive-contents)))
          (when (and in-archive
                     (version-list-< (get-version package package-alist)
                                     in-archive))
            (push (cadr (assq package package-archive-contents))
                  upgrades)))))
    (if upgrades
        (when (yes-or-no-p
               (message "Upgrade %d package%s (%s)? "
                        (length upgrades)
                        (if (= (length upgrades) 1) "" "s")
                        (mapconcat #'package-desc-full-name upgrades ", ")))
          (save-window-excursion
            (dolist (package-desc upgrades)
              (let ((old-package (cadr (assq (package-desc-name package-desc)
                                             package-alist))))
                (package-install package-desc)
                (package-delete  old-package)))))
      (message "All packages are up to date"))))



(defun message-project-root ()
  "Outputs project-root."
  (interactive)
  (let (project-dir (projectile-project-root))
    (if project-dir
        (message "Project dir found!")
      (message "No project-dir found."))))



;; Upgrade all packages
(defun package-upgrade-all ()
  "Upgrade all packages automatically without showing *Packages* buffer."
  (interactive)
  (package-refresh-contents)
  (let (upgrades)
    (cl-flet ((get-version (name where)
                           (let ((pkg (cadr (assq name where))))
                             (when pkg
                               (package-desc-version pkg)))))
      (dolist (package (mapcar #'car package-alist))
        (let ((in-archive (get-version package package-archive-contents)))
          (when (and in-archive
                     (version-list-< (get-version package package-alist)
                                     in-archive))
            (push (cadr (assq package package-archive-contents))
                  upgrades)))))
    (if upgrades
        (when (yes-or-no-p
               (message "Upgrade %d package%s (%s)? "
                        (length upgrades)
                        (if (= (length upgrades) 1) "" "s")
                        (mapconcat #'package-desc-full-name upgrades ", ")))
          (save-window-excursion
            (dolist (package-desc upgrades)
              (let ((old-package (cadr (assq (package-desc-name package-desc)
                                             package-alist))))
                (package-install package-desc)
                (package-delete  old-package)))))
      (message "All packages are up to date"))))
