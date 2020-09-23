;;; ~/code/depot/users/glittershark/emacs.d/clojure.el -*- lexical-binding: t; -*-

(defun clojure-thing-at-point-setup ()
  (interactive)
  ;; Used by cider-find-dwim to parse the symbol at point
  (setq-local
   thing-at-point-file-name-chars
   (concat thing-at-point-file-name-chars
           "><!?")))

(after! clojure-mode
  (define-clojure-indent
    (PUT 2)
    (POST 2)
    (GET 2)
    (PATCH 2)
    (DELETE 2)
    (context 2)
    (checking 3)
    (match 1)
    (domonad 0)
    (describe 1)
    (before 1)
    (it 2))

  (add-hook 'clojure-mode-hook #'clojure-thing-at-point-setup))

(use-package! flycheck-clojure
  ;; :disabled t
  :after (flycheck cider)
  :config
  (flycheck-clojure-setup))

(after! clj-refactor
  (setq cljr-magic-requires :prompt
        cljr-clojure-test-declaration "[clojure.test :refer :all]"
        cljr-cljc-clojure-test-declaration"#?(:clj [clojure.test :refer :all]
:cljs [cljs.test :refer-macros [deftest is testing]])"
        )
  (add-to-list
   'cljr-magic-require-namespaces
   '("s" . "clojure.spec.alpha")))

(set-popup-rule! "^\\*cider-test-report" :size 0.4)
