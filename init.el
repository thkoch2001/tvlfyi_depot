;;; private/grfn/init.el -*- lexical-binding: t; -*-

(doom! :completion
       company           ; the ultimate code completion backend
       ;;helm              ; the *other* search engine for love and life
       ;;ido               ; the other *other* search engine...
       ivy               ; a search engine for love and life

       :ui
       ;;deft              ; notational velocity for Emacs
       doom              ; what makes DOOM look the way it does
       ;doom-dashboard    ; a nifty splash screen for Emacs
       doom-quit         ; DOOM quit-message prompts when you quit Emacs
       ;fill-column       ; a `fill-column' indicator
       hl-todo           ; highlight TODO/FIXME/NOTE tags
       ;;indent-guides     ; highlighted indent columns
       modeline          ; snazzy, Atom-inspired modeline, plus API
       nav-flash         ; blink the current line after jumping
       ;;neotree           ; a project drawer, like NERDTree for vim
       ophints           ; highlight the region an operation acts on
       (popup            ; tame sudden yet inevitable temporary windows
        +all             ; catch all popups that start with an asterix
        +defaults)       ; default popup rules
       pretty-code       ; replace bits of code with pretty symbols
       ;;tabbar            ; FIXME an (incomplete) tab bar for Emacs
       ;;treemacs          ; a project drawer, like neotree but cooler
       unicode           ; extended unicode support for various languages
       vc-gutter         ; vcs diff in the fringe
       vi-tilde-fringe   ; fringe tildes to mark beyond EOB
       window-select     ; visually switch windows
       workspaces        ; tab emulation, persistence & separate workspaces

       :editor
       (evil +everywhere); come to the dark side, we have cookies
       file-templates    ; auto-snippets for empty files
       fold              ; (nigh) universal code folding
       ;;(format +onsave)  ; automated prettiness
       ;;lispy             ; vim for lisp, for people who dont like vim
       multiple-cursors  ; editing in many places at once
       ;;parinfer          ; turn lisp into python, sort of
       rotate-text       ; cycle region at point between text candidates
       snippets          ; my elves. They type so I don't have to

       :emacs
       (dired            ; making dired pretty [functional]
       ;;+ranger         ; bringing the goodness of ranger to dired
       ;;+icons          ; colorful icons for dired-mode
        )
       electric          ; smarter, keyword-based electric-indent
       ;;eshell            ; a consistent, cross-platform shell (WIP)
       ;;term              ; terminals in Emacs
       vc                ; version-control and Emacs, sitting in a tree

       :tools
       ;;ansible
       ;;debugger          ; FIXME stepping through code, to help you add bugs
       ;;direnv
       docker
       ;;editorconfig      ; let someone else argue about tabs vs spaces
       ;; ein               ; tame Jupyter notebooks with emacs
       eval              ; run code, run (also, repls)
       gist              ; interacting with github gists
       (lookup           ; helps you navigate your code and documentation
        +docsets)        ; ...or in Dash docsets locally
       ;;lsp
       ;;macos             ; MacOS-specific commands
       magit             ; a git porcelain for Emacs
       make              ; run make tasks from Emacs
       pass              ; password manager for nerds
       pdf               ; pdf enhancements
       ;;prodigy           ; FIXME managing external services & code builders
       ;;rgb               ; creating color strings
       ;;terraform         ; infrastructure as code
       ;;tmux              ; an API for interacting with tmux
       ;;upload            ; map local to remote projects via ssh/ftp
       ;;wakatime
       ;;vterm             ; another terminals in Emacs

       :checkers
       syntax          ; tasing you for every semicolon you forget
       spell          ; tasing you for misspelling mispelling

       :lang
       agda              ; types of types of types of types...
       ;;assembly          ; assembly for fun or debugging
       cc                ; C/C++/Obj-C madness
       clojure           ; java with a lisp
       ;;common-lisp       ; if you've seen one lisp, you've seen them all
       ; coq               ; proofs-as-programs
       ;;crystal           ; ruby at the speed of c
       ;;csharp            ; unity, .NET, and mono shenanigans
       data              ; config/data formats
       erlang            ; an elegant language for a more civilized age
       elixir            ; erlang done right
       ;;elm               ; care for a cup of TEA?
       emacs-lisp        ; drown in parentheses
       ;;ess               ; emacs speaks statistics
       ;;go                ; the hipster dialect
       ;; (haskell +intero) ; a language that's lazier than I am
       haskell ; a language that's lazier than I am
       ;;hy                ; readability of scheme w/ speed of python
       idris             ;
       ;;(java +meghanada) ; the poster child for carpal tunnel syndrome
       javascript        ; all(hope(abandon(ye(who(enter(here))))))
       julia             ; a better, faster MATLAB
       ;;kotlin            ; a better, slicker Java(Script)
       latex             ; writing papers in Emacs has never been so fun
       ;;ledger            ; an accounting system in Emacs
       ;;lua               ; one-based indices? one-based indices
       markdown          ; writing docs for people to ignore
       ;;nim               ; python + lisp at the speed of c
       nix               ; I hereby declare "nix geht mehr!"
       ;;ocaml             ; an objective camel
       (org              ; organize your plain life in plain text
        +dragndrop       ; drag & drop files/images into org buffers
        +attach          ; custom attachment system
        +babel           ; running code in org
        +capture         ; org-capture in and outside of Emacs
        +export          ; Exporting org to whatever you want
        ;; +habit           ; Keep track of your habits
        +present         ; Emacs for presentations
        +protocol)       ; Support for org-protocol:// links
       ;;perl              ; write code no one else can comprehend
       ;;php               ; perl's insecure younger brother
       ;;plantuml          ; diagrams for confusing people more
       purescript        ; javascript, but functional
       python            ; beautiful is better than ugly
       ;;qt                ; the 'cutest' gui framework ever
       racket            ; a DSL for DSLs
       rest              ; Emacs as a REST client
       ;;ruby              ; 1.step do {|i| p "Ruby is #{i.even? ? 'love' : 'life'}"}
       rust              ; Fe2O3.unwrap().unwrap().unwrap().unwrap()
       ;;scala             ; java, but good
       (sh +fish)        ; she sells (ba|z|fi)sh shells on the C xor
       ;;solidity          ; do you need a blockchain? No.
       ;;swift             ; who asked for emoji variables?
       ;;terra             ; Earth and Moon in alignment for performance.
       ;;web               ; the tubes
       ;;vala              ; GObjective-C

       ;; Applications are complex and opinionated modules that transform Emacs
       ;; toward a specific purpose. They may have additional dependencies and
       ;; should be loaded late.
       :app
       ;;(email +gmail)    ; emacs as an email client
       irc               ; how neckbeards socialize
       ;;(rss +org)        ; emacs as an RSS reader
       twitter           ; twitter client https://twitter.com/vnought
       ;;(write            ; emacs as a word processor (latex + org + markdown)
       ;; +wordnut         ; wordnet (wn) search
       ;; +langtool)       ; a proofreader (grammar/style check) for Emacs

       :email
       (mu4e +gmail)
       notmuch

       :collab
       ;;floobits          ; peer programming for a price
       ;;impatient-mode    ; show off code over HTTP

       :config
       ;; For literate config users. This will tangle+compile a config.org
       ;; literate config in your `doom-private-dir' whenever it changes.
       ;;literate

       ;; The default module sets reasonable defaults for Emacs. It also
       ;; provides a Spacemacs-inspired keybinding scheme and a smartparens
       ;; config. Use it as a reference for your own modules.
       (default +bindings +smartparens))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(doom-big-font-mode nil)
 '(flycheck-javascript-flow-args nil)
 '(org-agenda-files
   '("/home/griffin/notes/personal.org" "/home/griffin/notes/2020-01-27-data-pipeline-deploy-mismatch.org" "/home/griffin/notes/architecture.org" "/home/griffin/notes/cooking.org" "/home/griffin/notes/culture-survey.org" "/home/griffin/notes/dir-structure.org" "/home/griffin/notes/dnd.org" "/home/griffin/notes/inbox.org" "/home/griffin/notes/misc-todo.org" "/home/griffin/notes/nix-talk.org" "/home/griffin/notes/notes.org" "/home/griffin/notes/one-on-one.org" "/home/griffin/notes/work.org" "/home/griffin/notes/xanthous.org" "/home/griffin/notes/xgboost.org"))
 '(safe-local-variable-values
   '((intero-stack-yaml . "/home/griffin/code/mlem/stack.yaml")
     (elisp-lint-indent-specs
      (if-let* . 2)
      (when-let* . 1)
      (let* . defun)
      (nrepl-dbind-response . 2)
      (cider-save-marker . 1)
      (cider-propertize-region . 1)
      (cider-map-repls . 1)
      (cider--jack-in . 1)
      (cider--make-result-overlay . 1)
      (insert-label . defun)
      (insert-align-label . defun)
      (insert-rect . defun)
      (cl-defun . 2)
      (with-parsed-tramp-file-name . 2)
      (thread-first . 1)
      (thread-last . 1))
     (checkdoc-package-keywords-flag)
     (cider-jack-in-default . "shadow-cljs")
     (projectile-project-root . "/home/griffin/code/urb/grid/backend/src")
     (python-pytest-executable . "/home/griffin/code/urb/grid/backend/src/.venv/bin/pytest"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((((class color) (min-colors 89)) (:foreground "#657b83" :background "#fdf6e3"))))
 '(agda2-highlight-bound-variable-face ((t nil)))
 '(agda2-highlight-coinductive-constructor-face ((t (:foreground "#b58900"))))
 '(agda2-highlight-datatype-face ((t (:foreground "#268bd2"))))
 '(agda2-highlight-dotted-face ((t nil)))
 '(agda2-highlight-error-face ((t (:foreground "#dc322f" :underline t))))
 '(agda2-highlight-field-face ((t (:foreground "#dc322f"))))
 '(agda2-highlight-function-face ((t (:foreground "#268bd2"))))
 '(agda2-highlight-incomplete-pattern-face ((t (:background "#cb4b16" :foreground "#002b36"))))
 '(agda2-highlight-inductive-constructor-face ((t (:foreground "#859900"))))
 '(agda2-highlight-keyword-face ((t (:foreground "#859900"))))
 '(agda2-highlight-module-face ((t (:foreground "#b58900"))))
 '(agda2-highlight-number-face ((t (:foreground "#6c71c4"))))
 '(agda2-highlight-operator-face ((t nil)))
 '(agda2-highlight-postulate-face ((t (:foreground "#268bd2"))))
 '(agda2-highlight-primitive-face ((t (:foreground "#268bd2"))))
 '(agda2-highlight-primitive-type-face ((t (:foreground "#268bd2"))))
 '(agda2-highlight-record-face ((t (:foreground "#268bd2"))))
 '(agda2-highlight-string-face ((t (:foreground "#2aa198"))))
 '(agda2-highlight-symbol-face ((((background "#fdf6e3")) (:foreground "#586e75"))))
 '(agda2-highlight-termination-problem-face ((t (:background "#cb4b16" :foreground "#002b36"))))
 '(agda2-highlight-typechecks-face ((t (:background "#2aa198" :foreground "#002b36"))))
 '(agda2-highlight-unsolved-constraint-face ((t (:background "#eee8d5"))))
 '(agda2-highlight-unsolved-meta-face ((t (:background "#eee8d5")))))
