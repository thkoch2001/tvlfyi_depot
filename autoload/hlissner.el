;;; autoload/hlissner.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +hlissner/install-snippets ()
  "Install my snippets from https://github.com/hlissner/emacs-snippets into
private/hlissner/snippets."
  (interactive)
  (doom-fetch :github "hlissner/emacs-snippets"
              (expand-file-name "snippets" (doom-module-path :private 'hlissner))))

;;;###autoload
(defun +hlissner/yank-buffer-filename ()
  "Copy the current buffer's path to the kill ring."
  (interactive)
  (if-let* ((filename (or buffer-file-name (bound-and-true-p list-buffers-directory))))
      (message (kill-new (abbreviate-file-name filename)))
    (error "Couldn't find filename in current buffer")))

(defmacro +hlissner-def-finder! (name dir)
  "Define a pair of find-file and browse functions."
  `(progn
     (defun ,(intern (format "+hlissner/find-in-%s" name)) ()
       (interactive)
       (let ((default-directory ,dir)
             projectile-project-name
             projectile-require-project-root
             projectile-cached-buffer-file-name
             projectile-cached-project-root)
         (call-interactively (command-remapping #'projectile-find-file))))
     (defun ,(intern (format "+hlissner/browse-%s" name)) ()
       (interactive)
       (let ((default-directory ,dir))
         (call-interactively (command-remapping #'find-file))))))

;;;###autoload (autoload '+hlissner/find-in-templates "autoload/hlissner" nil t)
;;;###autoload (autoload '+hlissner/browse-templates "autoload/hlissner" nil t)
(+hlissner-def-finder! templates +file-templates-dir)

;;;###autoload (autoload '+hlissner/find-in-snippets "autoload/hlissner" nil t)
;;;###autoload (autoload '+hlissner/browse-snippets "autoload/hlissner" nil t)
(+hlissner-def-finder! snippets +hlissner-snippets-dir)

;;;###autoload (autoload '+hlissner/find-in-dotfiles "autoload/hlissner" nil t)
;;;###autoload (autoload '+hlissner/browse-dotfiles "autoload/hlissner" nil t)
(+hlissner-def-finder! dotfiles (expand-file-name ".dotfiles" "~"))

;;;###autoload (autoload '+hlissner/find-in-emacsd "autoload/hlissner" nil t)
;;;###autoload (autoload '+hlissner/browse-emacsd "autoload/hlissner" nil t)
(+hlissner-def-finder! emacsd doom-emacs-dir)

;;;###autoload (autoload '+hlissner/find-in-notes "autoload/hlissner" nil t)
;;;###autoload (autoload '+hlissner/browse-notes "autoload/hlissner" nil t)
(+hlissner-def-finder! notes +org-dir)
