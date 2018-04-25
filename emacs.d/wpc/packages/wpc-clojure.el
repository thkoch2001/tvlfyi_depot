;;; clojure.el --- My Clojure preferences -*- lexical-binding: t -*-
;; Author: William Carroll <wpcarro@gmail.com>

;;; Commentary:
;; Hosting my Clojure tooling preferences

;;; Code:

;; Helper functions
(defun wpc/buffer-name-for-clojure-mode (mode)
  (let* ((project-name (projectile-project-name))
         (cljs-name (concat "*cider-repl CLJS " project-name "*"))
         (clj-name  (concat "*cider-repl " project-name "*")))
    (cond ((eq mode 'clojurescript-mode) cljs-name)
          ((eq mode 'clojure-mode) clj-name)
          ((eq mode 'clojurec-mode) cljs-name))))

(defun wpc/repl-function-for-clojure-mode (mode)
  (let ((project-name (projectile-project-name))
        (cljs-fn #'cider-jack-in-clojurescript)
        (clj-fn  #'cider-jack-in))
    (cond ((eq mode 'clojurescript-mode) cljs-fn)
          ((eq mode 'clojure-mode) clj-fn)
          ((eq mode 'clojurec-mode) cljs-fn))))

(defun wpc/find-or-create-clojure-or-clojurescript-repl ()
  (interactive)
  (with-current-buffer (current-buffer)
    (let ((buffer-name   (wpc/buffer-name-for-clojure-mode major-mode))
          (repl-function (wpc/repl-function-for-clojure-mode major-mode)))
      (if (get-buffer buffer-name)
          (switch-to-buffer buffer-name)
        (funcall repl-function)))))

;; (defun wpc/evil-leader/set-key-for-clojure-modes (kbd callback)
;;   (evil-leader/set-key-for-mode 'clojure-mode kbd callback)
;;   (evil-leader/set-key-for-mode 'clojurec-mode kbd callback)
;;   (evil-leader/set-key-for-mode 'clojurescript-mode kbd callback))

;; ;; clojure
;; (wpc/evil-leader/set-key-for-clojure-modes "d" #'cider-doc)
;; (wpc/evil-leader/set-key-for-clojure-modes "e" #'cider-eval-defun-at-point)
;; (wpc/evil-leader/set-key-for-clojure-modes "r" #'wpc/find-or-create-clojure-or-clojurescript-repl)

(use-package cider
  :general
  (cider-repl-mode-map
   "C-l"    'cider-repl-clear-buffer
   "C-u"    'kill-whole-line
   "<up>"   'cider-repl-previous-input
   "<down>" 'cider-repl-next-input
   "C-c 'j" 'wpc/find-or-create-clojure-or-clojurescript-repl)
  (n
   "M-." 'cider-find-var)
  :config
  (setq cider-cljs-lein-repl
        "(do (require 'figwheel-sidecar.repl-api)
             (figwheel-sidecar.repl-api/start-figwheel!)
             (figwheel-sidecar.repl-api/cljs-repl))"
        cider-prompt-for-symbol nil))

(provide 'wpc-clojure)
;;; wpc-clojure.el ends here
