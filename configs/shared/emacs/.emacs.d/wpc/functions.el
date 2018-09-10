;; functions.el --- Helper functions for my Emacs development -*- lexical-binding: t -*-
;; Author: William Carroll <wpcarro@gmail.com>

;;; Commentary:
;; This file hopefully contains friendly APIs that making ELisp development more enjoyable.

;;; Code:

;; TODO: clean up this file so this isn't necessary
(setq evil-want-integration nil)
(require 'evil)

(require 'projectile)
(require 'paredit)
(require 'term)
(require 'f)
(require 'yasnippet)
(require 'ido)

(defun wpc/evil-window-vsplit-right ()
  (interactive)
  (evil-window-vsplit)
  (windmove-right))

(defun wpc/evil-window-split-down ()
  (interactive)
  (evil-window-split)
  (windmove-down))

(defun wpc/reindent-defun-and-align-clojure-map ()
  (interactive)
  (call-interactively #'paredit-reindent-defun)
  (call-interactively #'clojure-align))

(defun wpc/find-file ()
  "Prefer project-based file-finding if inside of project; otherwise gracefully fallback."
  (interactive)
  (with-current-buffer (current-buffer)
    (if (projectile-project-p)
        (call-interactively #'projectile-find-file)
      (call-interactively #'find-file))))

(defun wpc/find-or-create-js-test ()
  (->> buffer-file-name
       (s-chop-suffix ".js")
       (s-append ".test.js")
       (find-file)))

(defun wpc/find-or-create-js-module ()
  (->> buffer-file-name
       (s-chop-suffix ".test.js")
       (s-append ".js")
       (find-file)))

(defun wpc/find-or-create-js-store ()
  (->> buffer-file-name
       (s-replace "index.js" "store.js")
       (find-file)))

(defun wpc/find-or-create-js-component ()
  (->> buffer-file-name
       (s-replace "store.js" "index.js")
       (find-file)))

(defun wpc/bind-ido-keys ()
  "Adds custom KBDs for ido. This function is recommended in the ido source code."
  (define-key ido-completion-map (kbd "<tab>") #'ido-next-match)
  (define-key ido-completion-map (kbd "<backtab>") #'ido-prev-match))

(defun wpc/toggle-between-js-test-and-module ()
  "Toggle between a Javascript test or module."
  (interactive)
  (if (s-ends-with? ".test.js" buffer-file-name)
      (wpc/find-or-create-js-module)
    (if (s-ends-with? ".js" buffer-file-name)
        (wpc/find-or-create-js-test)
      (message "Not in a Javascript file. Exiting..."))))

(defun wpc/toggle-between-js-component-and-store ()
  "Toggle between a React component and its Redux store."
  (interactive)
  (if (s-ends-with? "index.js" buffer-file-name)
      (wpc/find-or-create-js-store)
    (if (or (s-ends-with? "store.js" buffer-file-name)
            (s-ends-with? "store.test.js" buffer-file-name))
        (wpc/find-or-create-js-component)
      (message "Not in a React/Redux file. Exiting..."))))

(defun wpc/read-file-as-string (filename)
  (with-temp-buffer
    (insert-file-contents filename)
    (s-trim (buffer-string))))

(defun wpc/create-snippet ()
  "Creates a window split and then opens the Yasnippet editor."
  (interactive)
  (evil-window-vsplit)
  (call-interactively #'yas-new-snippet))

(defun wpc/edit-init-el ()
  "Creates a window split and then edits the init.el file."
  (interactive)
  (evil-window-vsplit)
  (find-file "~/.emacs.d/init.el"))

(defun wpc/jump-to-parent-file ()
  "Jumps to a React store or component's parent file. Useful for store or index file."
  (interactive)
  (-> buffer-file-name
      f-dirname
      (f-join "..")
      (f-join (f-filename buffer-file-name))
      find-file))

(defun wpc/tmux-emacs-windmove (dir)
  "Move windows in a Tmux-friendly way."
  (let* ((dir->opts '((left . ("-L" . windmove-left))
                      (right . ("-R" . windmove-right))
                      (above . ("-U" . windmove-up))
                      (below . ("-D" . windmove-down))))
         (opts (alist-get dir dir->opts))
         (tmux-opt (car opts))
         (emacs-fn (cdr opts)))
    (if (window-in-direction dir)
        (funcall emacs-fn)
      (shell-command (format "tmux select-pane %s" tmux-opt)))))

(defun wpc/tmux-emacs-windmove-left ()
  (interactive)
  (wpc/tmux-emacs-windmove 'left))

(defun wpc/tmux-emacs-windmove-right ()
  (interactive)
  (wpc/tmux-emacs-windmove 'right))

(defun wpc/tmux-emacs-windmove-up ()
  (interactive)
  (wpc/tmux-emacs-windmove 'above))

(defun wpc/tmux-emacs-windmove-down ()
  (interactive)
  (wpc/tmux-emacs-windmove 'below))

(defun wpc/get-window-by-buffername (buffername)
  "Finds a window by the name of the buffer it's hosting."
  (let ((buffer (get-buffer buffername)))
    (when buffer
        (get-buffer-window buffer))))

(defun wpc/add-earmuffs (x)
  "Returns X surrounded by asterisks."
  (format "*%s*" x))

(defun wpc/get-default-shell ()
  (or explicit-shell-file-name
      (getenv "SHELL")
      (getenv "ESHELL")))

(defun wpc/find-terminal-buffer ()
  (get-buffer (wpc/add-earmuffs wpc/terminal-name)))

(defun wpc/find-terminal-window ()
  (wpc/get-window-by-buffername (wpc/add-earmuffs wpc/terminal-name)))

(defun wpc/create-terminal-session ()
  (wpc/evil-window-vsplit-right)
  (ansi-term (wpc/get-default-shell) wpc/terminal-name))

(defun wpc/toggle-terminal ()
  "Toggles a custom terminal session in Emacs."
  (interactive)
  (let ((window (wpc/find-terminal-window)))
    (if window
        (delete-window window)
      (wpc/find-or-create-terminal))))

(defun wpc/find-or-create-terminal ()
  (let ((buffer (wpc/find-terminal-buffer)))
    (if buffer
        (display-buffer buffer)
      (wpc/create-terminal-session))))

(defun wpc/put-file-name-on-clipboard ()
  "Put the current file name on the clipboard"
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (with-temp-buffer
        (insert filename)
        (clipboard-kill-region (point-min) (point-max)))
      (message filename))))

(defun wpc/evil-replace-under-point ()
  "Faster than typing %s//thing/g"
  (interactive)
  (save-excursion
    (evil-ex (concat "%s/\\b" (symbol-name (symbol-at-point)) "\\b/"))))

(defun wpc/disable-linum-mode ()
  "Convenience function defined to make adding hooks easier without a lambda."
  (linum-mode -1))

(defun wpc/disable-company-mode ()
  "Convenience function defined to make adding hooks easier without a lambda."
  (company-mode -1))

(defun wpc/toggle-term-mode ()
  "Toggle between term-line-mode and temr-char-mode."
  (if (term-in-line-mode)
      (term-char-mode)
    (term-line-mode)))

(defun buffer-dirname ()
  "Return the directory name of the current buffer as a string."
  (->> buffer-file-name
       f-dirname
       f-filename))

(provide 'functions)
;;; functions.el ends here
