(defun wc/projectile-shell-pop ()
  "Opens `ansi-term' at the project root according to Projectile."
  (interactive)
  (let ((default-directory (projectile-project-root)))
    (if (get-buffer "*ansi-term*")
        (switch-to-buffer "*ansi-term*")
      (ansi-term "/bin/zsh"))
    (term-send-string (terminal) (format "cd '%s'\n" default-directory))
    (get-buffer-process "*ansi-term*")))


(defun wc/join-erc ()
  "Boots `erc' and autojoins channels."
  (interactive)
  (erc :server "irc.freenode.net" :port "6667" :nick "wpcarro"))


(defun wc/bootstrap-ansi-term ()
  "Custom `ansi-term' configuration."
  (interactive)
  (linum-mode nil)
  (local-set-key (kbd "C-h") 'evil-window-left)
  (local-set-key (kbd "C-l") 'evil-window-right)
  (local-set-key (kbd "C-k") 'evil-window-up)
  (local-set-key (kbd "C-j") 'evil-window-down)
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
