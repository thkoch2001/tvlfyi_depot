;;; functions.el --- Helper functions -*- lexical-binding: t -*-

;; Author: William Carroll <wpcarro@gmail.com>
;; Version: 0.0.1
;; URL: https://git.wpcarro.dev/wpcarro/briefcase
;; Package-Requires: ((emacs "24"))

;;; Commentary:
;; This file hopefully contains friendly APIs that making ELisp development more
;; enjoyable.

;; TODO: Break these out into separate modules.

;;; Code:
(defun functions-evil-window-vsplit-right ()
  (interactive)
  (evil-window-vsplit)
  (windmove-right))

(defun functions-evil-window-split-down ()
  (interactive)
  (evil-window-split)
  (windmove-down))

(defun functions-create-snippet ()
  "Creates a window split and then opens the Yasnippet editor."
  (interactive)
  (evil-window-vsplit)
  (call-interactively #'yas-new-snippet))

(defun functions-evil-replace-under-point ()
  "Faster than typing %s//thing/g."
  (interactive)
  (let ((term (s-replace "/" "\\/" (symbol-to-string (symbol-at-point)))))
    (save-excursion
      (evil-ex (concat "%s/\\b" term "\\b/")))))

(defun functions-buffer-dirname ()
  "Return the directory name of the current buffer as a string."
  (->> buffer-file-name
       f-dirname
       f-filename))

(provide 'functions)
;;; functions.el ends here
