;;; clojure.el --- My Clojure preferences -*- lexical-binding: t -*-
;; Author: William Carroll <wpcarro@gmail.com>

;;; Commentary:
;; Hosting my Clojure tooling preferences

;;; Code:

;; Helper functions

;; (defun wpc/buffer-name-for-clojure-mode (mode)
;;   (let* ((project-name (projectile-project-name))
;;          (cljs-name (concat "*cider-repl CLJS " project-name "*"))
;;          (clj-name  (concat "*cider-repl " project-name "*")))
;;     (cond ((eq mode 'clojurescript-mode) cljs-name)
;;           ((eq mode 'clojure-mode) clj-name)
;;           ((eq mode 'clojurec-mode) cljs-name))))

;; (defun wpc/repl-function-for-clojure-mode (mode)
;;   (let ((project-name (projectile-project-name))
;;         (cljs-fn #'cider-jack-in-clojurescript)
;;         (clj-fn  #'cider-jack-in))
;;     (cond ((eq mode 'clojurescript-mode) cljs-fn)
;;           ((eq mode 'clojure-mode) clj-fn)
;;           ((eq mode 'clojurec-mode) cljs-fn))))

;; (defun wpc/find-or-create-clojure-or-clojurescript-repl ()
;;   (interactive)
;;   (with-current-buffer (current-buffer)
;;     (let ((buffer-name   (wpc/buffer-name-for-clojure-mode major-mode))
;;           (repl-function (wpc/repl-function-for-clojure-mode major-mode)))
;;       (if (get-buffer buffer-name)
;;           (switch-to-buffer buffer-name)
;;         (funcall repl-function)))))

(use-package clojure-mode
  :config
  ;; from Ryan Schmukler:
  (setq cljr-magic-require-namespaces
        '(("io" . "clojure.java.io")
          ("sh" . "clojure.java.shell")
          ("jdbc" . "clojure.java.jdbc")
          ("set" . "clojure.set")
          ("time" . "java-time")
          ("str" . "cuerdas.core")
          ("path" . "pathetic.core")
          ("walk" . "clojure.walk")
          ("zip" . "clojure.zip")
          ("async" . "clojure.core.async")
          ("component" . "com.stuartsierra.component")
          ("http" . "clj-http.client")
          ("url" . "cemerick.url")
          ("sql" . "honeysql.core")
          ("csv" . "clojure.data.csv")
          ("json" . "cheshire.core")
          ("s" . "clojure.spec.alpha")
          ("fs" . "me.raynes.fs")
          ("ig" . "integrant.core")
          ("cp" . "com.climate.claypoole")
          ("re-frame" . "re-frame.core")
          ("rf" . "re-frame.core")
          ("re" . "reagent.core")
          ("reagent" . "reagent.core")
          ("u.core" . "utopia.core")
          ("gen" . "clojure.spec.gen.alpha"))))

(use-package cider
  :config
  (general-define-key
    :keymaps 'cider-repl-mode-map
    "C-l"    #'cider-repl-clear-buffer
    "C-u"    #'kill-whole-line
    "<up>"   #'cider-repl-previous-input
    "<down>" #'cider-repl-next-input
    ;; "C-c 'j" #'wpc/find-or-create-clojure-or-clojurescript-repl
    )
  ;; (setq cider-cljs-lein-repl
  ;;       "(do (require 'figwheel-sidecar.repl-api)
  ;;            (figwheel-sidecar.repl-api/start-figwheel!)
  ;;            (figwheel-sidecar.repl-api/cljs-repl))"
  ;;       cider-prompt-for-symbol nil)
  )

(provide 'wpc-clojure)
;;; wpc-clojure.el ends here
