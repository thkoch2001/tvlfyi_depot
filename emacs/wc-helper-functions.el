(require 'cl)

(defun wc/open-in-pager (file)
  "Opens a file in a simulated pager in emacs."
  (find-file file)
  (emacs-pager-mode))


(defun wc/write-quit-kill-buffer ()
  "Writes, quits, kills a buffer."
  (interactive)
  (save-buffer)
  (kill-this-buffer))


;; (defun wc/edit-file-in-emacs (file)
;;   "Edits a file in a buffer in Emacs. On :wq, the buffer is deleted and the previous term session restored."
;;   (find-file file)
;;   (quick-edit-file-mode))


;; (defvar quick-edit-file-mode-map
;;   (let ((map (make-sparse-keymap)))
;;     (define-key map (kbd "q") 'wc/write-quit-kill-buffer)
;;     map)
;;   "Keymap for emacs quick-edit file mode.")


;; (define-derived-mode quick-edit-file-mode fundamental-mode "QuickEdit"
;;   "Mode quickly editing files."
;;   (setq-local make-backup-files nil)
;;   (setq buffer-name "*quick-edit*"))


;; (defun wc/quick-edit-evil-quit (old-fun &rest args)
;;   (if (eq major-mode 'quick-edit-file-mode)
;;       (wc/write-quit-kill-buffer)
;;     (apply old-fun args)))


;; (defadvice evil-quit
;;     (around wc/quick-edit-quick activate)
;;   (wc/quick-edit-evil-quit))


(defvar emacs-pager-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") 'kill-this-buffer)
    map)
  "Keymap for emacs pager mode.")


(defcustom emacs-pager-max-line-coloring 500
  "Maximum number of lines to ansi-color. If performance is bad when
   loading data, reduce this number"
  :group 'emacs-pager)


(define-derived-mode emacs-pager-mode fundamental-mode "Pager"
  "Mode for viewing data paged by emacs-pager"
  (setq-local make-backup-files nil)
  (ansi-color-apply-on-region (goto-char (point-min))
                              (save-excursion
                                (forward-line emacs-pager-max-line-coloring)
                                (point)))
  (setq buffer-name "*pager*")
  (set-buffer-modified-p nil)
  (read-only-mode)
  (evil-define-key 'normal emacs-pager-mode-map
    (kbd "q") 'kill-this-buffer
    (kbd "ESC") 'kill-this-buffer))


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


(defun wc/shell-history ()
  (setq history (shell-command-to-string "history"))
  (split-string history "\n"))


(defun wc/git-branches ()
  (setq branches (shell-command-to-string "git branch -a | tr -d '* ' | sed 's/^remotes\\/origin\\///' | sort | uniq"))
  (split-string branches "\n"))


(defun wc/autojump-directories ()
  (setq directories (shell-command-to-string "j -s | awk '{ if($2 ~ /^\\// && $1 != \"data:\") print;}' | sort -rn | head -n 100 | awk '{print $2}'"))
  (split-string directories "\n"))


(defun wc/handle-branch (branch)
  (setq action "git diff")
  (term-send-raw-string (format "%s %s" action branch)))


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
  (define-key term-raw-map (kbd "C-c") 'term-interrupt-subjob)
  (define-key term-raw-map (kbd "C-h") 'windmove-left)
  (define-key term-raw-map (kbd "C-l") 'windmove-right)
  (define-key term-raw-map (kbd "C-k") 'windmove-up)
  (define-key term-raw-map (kbd "C-j") 'windmove-down)
  (wc/expose-global-binding-in-term (kbd "M-x"))
  (evil-define-key 'normal term-raw-map
    (kbd "i") 'wc/focus-term-at-bottom)
  (define-key term-raw-map (kbd "C-r") 'wc/helm-shell-history)
  (define-key term-raw-map (kbd "C-t") 'wc/helm-ctrl-t-find-files)
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
  (setq current-buffer-name (buffer-name (current-buffer)))
  (setq buffer-candidates (remove-if #'(lambda (buffer) (string-match-p current-buffer-name (buffer-name buffer))) (buffer-list)))
  (wc/do-switch-to-mru-buffer buffer-candidates))


(defun wc/do-switch-to-mru-buffer (buffer-candidates)
  (setq buffer-candidate (car buffer-candidates))
  (setq rest (cdr buffer-candidates))
  (if (string-match-p current-buffer-name (buffer-name buffer-candidate))
      (wc/do-switch-to--buffer rest)
    (if (eq 0 (list-length buffer-candidates))
        (message "No more buffer candidates.")
      (if (wc/file-buffer-p buffer-candidate)
          (switch-to-buffer buffer-candidate)
        (wc/do-switch-to-mru-buffer rest)))))


(defun wc/file-buffer-p (buffer-candidate)
  "Returns t if the buffer argument is backed by a file and is therefore presumably a code buffer."
  (interactive)
  (let ((buff-name (buffer-name buffer-candidate))
        (buff-mode (wc/buffer-major-mode buffer-candidate)))
    (not (or (string-match-p "*" buff-name)
             (member buff-mode '(neotree-mode dired-mode))))))


(defun wc/buffer-major-mode (buffer-handle)
  "Returns a buffer's active major-mode."
  (with-current-buffer buffer-handle major-mode))


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
