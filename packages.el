;; -*- no-byte-compile: t; -*-
;;; private/grfn/packages.el

;; (package! 'tide :disable t)

;; Editor
(package! solarized-theme)
(package! fill-column-indicator)
(package! flx)
(package! general
  :recipe (general
           :fetcher github
           :repo "noctuid/general.el"))
(package! org-clubhouse
  :recipe (org-clubhouse
           :fetcher file
           :path "~/code/urb/org-clubhouse"))
(package! fill-column-indicator)
(package! writeroom-mode)
(package! dash)

;; Presentation
(package! epresent)
(package! org-tree-slide)
(package! ox-reveal)

;; Slack etc
(package! slack)
(package! alert)

;; Git
(package! evil-magit)
(package! magithub)
(package! magit-gh-pulls)
(package! marshal)
; (package! auth-password-store)

;; Elisp
(package! dash)
(package! dash-functional)
(package! s)
(package! request)

;; Haskell
(package! lsp-mode)
(package! lsp-ui :recipe (:fetcher github :repo "emacs-lsp/lsp-ui"))
(package! lsp-haskell)
(package! company-lsp)

;; Rust
(package! cargo)

;; Elixir
(package! flycheck-credo)
(package! flycheck-mix)
(package! flycheck-dialyxir)

;; Lisp
(package! paxedit)

;; Javascript
(package! flow-minor-mode)
(package! flycheck-flow)
(package! company-flow)
(package! prettier-js)

;; GraphQL
(package! graphql-mode)

;; Haskell
;; (package! lsp-mode)
;; (package! lsp-ui)
;; (package! lsp-haskell)
;; (package! company-lsp)
;; (package! lsp-imenu)

;; Clojure
(package! flycheck-clojure)

;; SQL
(package! sqlup-mode)
