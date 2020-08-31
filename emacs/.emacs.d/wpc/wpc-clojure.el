;;; wpc-clojure.el --- My Clojure preferences -*- lexical-binding: t -*-

;; Author: William Carroll <wpcarro@gmail.com>
;; Version: 0.0.1
;; URL: https://git.wpcarro.dev/wpcarro/briefcase
;; Package-Requires: ((emacs "25.1"))

;;; Commentary:
;; Hosting my Clojure tooling preferences

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
    "<down>" #'cider-repl-next-input)
  (general-define-key
   :keymaps 'clojure-mode-map
   :states '(normal)
   :prefix "<SPC>"
   "x" #'cider-eval-defun-at-point
   "X" #'cider-eval-buffer
   "d" #'cider-symbol-at-point)
  (setq cider-prompt-for-symbol nil))

(provide 'wpc-clojure)
;;; wpc-clojure.el ends here
