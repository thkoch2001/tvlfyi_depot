;; -*- no-byte-compile: t; -*-
;;; private/grfn/packages.el

(package! moody)

;; Editor
(package! solarized-theme)
(package! fill-column-indicator)
(package! flx)
(package! general
  :recipe (:host github :repo "noctuid/general.el"))
(package! fill-column-indicator)
(package! writeroom-mode)
(package! dash)
(package! w3m)
(package! rainbow-mode)
(package! string-inflection)

;;; Org
(package! org-clubhouse
  :recipe (:host file
           :local-repo "~/code/org-clubhouse"))
(package! org-alert)
(package! ob-http)
(package! ob-ipython)
(package! ob-async)
(package! org-recent-headings)
(package! org-sticky-header)
(package! gnuplot)
(package! gnuplot-mode)

;; Presentation
(package! epresent)
(package! org-tree-slide)
(package! ox-reveal)

;; Slack etc
(package! slack)
(package! alert)

;; Git
(package! evil-magit)
(package! marshal)
(package! forge)
(package!
  github-review
  :recipe
  (:host github
         :repo "charignon/github-review"
         :files ("github-review.el")))

;; Elisp
(package! dash)
(package! dash-functional)
(package! s)
(package! request)
(package! predd
  :recipe (:host github :repo "skeeto/predd"))

;; Haskell
(package! lsp-haskell)
(package! counsel-etags)

;;; LSP
(package! lsp-mode)
(package! lsp-ui :recipe (:host github :repo "emacs-lsp/lsp-ui"))
(package! company-lsp)
(package! lsp-treemacs)
(package! dap-mode)

;; Rust
(package! rustic :disable t)
(package! racer :disable t)
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
(package! lsp-mode)
(package! lsp-ui)
(package! lsp-haskell)
(package! company-lsp)
;; (package! lsp-imenu)

;; Clojure
(package! flycheck-clojure)

;; SQL
(package! sqlup-mode)
(package! emacsql)
(package! emacsql-psql)

;;; Python
(package! pyimport)
;; (package! yapfify)
(package! blacken)


;;; Desktop interaction
(package! counsel-spotify)

;;; Dhall
(package! dhall-mode)

;;; Kubernetes
(package! kubernetes)
(package! kubernetes-evil)
(package! k8s-mode)

;;; Stack Exchange
(package! sx)

;;; Nix
(package! nix-update
  :recipe (:host github
           :repo "glittershark/nix-update-el"))
(package! direnv)

;;; Email
(package! mu4e)

;;; Sequence diagrams
(package! wsd-mode
  :recipe (:host github
           :repo "josteink/wsd-mode"))

;;; logic?
(package! metal-mercury-mode
  :recipe (:host github
                 :repo "ahungry/metal-mercury-mode"))
(package! flycheck-mercury)

(package! terraform-mode)
(package! company-terraform)

;;;
(package! znc
  :recipe (:host github
                 :repo "sshirokov/ZNC.el"))
