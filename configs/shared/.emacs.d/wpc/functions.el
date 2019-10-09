;; functions.el --- Helper functions for my Emacs development -*- lexical-binding: t -*-
;; Author: William Carroll <wpcarro@gmail.com>

;;; Commentary:
;; This file hopefully contains friendly APIs that making ELisp development more
;; enjoyable.

;; TODO: Break these out into separate modules.

;;; Code:
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
        (call-interactively #'counsel-projectile-find-file)
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

(defun wpc/find-file-split (filename)
  "Creates a window split and then edits `filename'."
  (interactive)
  (evil-window-vsplit)
  (find-file filename))

(defun wpc/jump-to-parent-file ()
  "Jumps to a React store or component's parent file. Useful for store or index file."
  (interactive)
  (-> buffer-file-name
      f-dirname
      (f-join "..")
      (f-join (f-filename buffer-file-name))
      find-file))

(defun wpc/add-earmuffs (x)
  "Returns X surrounded by asterisks."
  (format "*%s*" x))

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

(s-replace "/" "x" "a/b/c")

(defun wpc/evil-replace-under-point ()
  "Faster than typing %s//thing/g."
  (interactive)
  (let ((term (s-replace "/" "\\/" (symbol/to-string (symbol-at-point)))))
    (save-excursion
      (evil-ex (concat "%s/\\b" term "\\b/")))))

(defun buffer-dirname ()
  "Return the directory name of the current buffer as a string."
  (->> buffer-file-name
       f-dirname
       f-filename))

(provide 'functions)
;;; functions.el ends here
