;;; private/grfn/init.el -*- lexical-binding: t; -*-


(doom! :feature
      ;debugger          ; FIXME stepping through code, to help you add bugs
       eval              ; run code, run (also, repls)
       (evil +everywhere); come to the dark side, we have cookies
       file-templates    ; auto-snippets for empty files
       (lookup           ; helps you navigate your code and documentation
        +devdocs         ; ...on devdocs.io online
        +docsets)        ; ...or in Dash docsets locally
       snippets          ; my elves. They type so I don't have to
       spellcheck        ; tasing you for misspelling mispelling
       (syntax-checker   ; tasing you for every semicolon you forget
        +childframe)     ; use childframes for error popups (Emacs 26+ only)
       workspaces        ; tab emulation, persistence & separate workspaces

       :completion
       (company          ; the ultimate code completion backend
        +auto)           ; as-you-type code completion
      ;(helm             ; the *other* search engine for love and life
      ; +fuzzy)          ; enable fuzzy search backend for helm
      ;ido               ; the other *other* search engine...
       (ivy              ; a search engine for love and life
        +fuzzy)          ; enable fuzzy search backend for ivy

       :ui
       doom              ; what makes DOOM look the way it does
       doom-dashboard    ; a nifty splash screen for Emacs
       doom-modeline     ; a snazzy Atom-inspired mode-line
       doom-quit         ; DOOM quit-message prompts when you quit Emacs
       evil-goggles      ; display visual hints when editing in evil
      ;fci               ; a `fill-column' indicator
       hl-todo           ; highlight TODO/FIXME/NOTE tags
      ;modeline          ; snazzy, Atom-inspired modeline, plus API
       nav-flash         ; blink the current line after jumping
      ;neotree           ; a project drawer, like NERDTree for vim
      ;treemacs          ; a project drawer, like neotree but cooler
       (popup            ; tame sudden yet inevitable temporary windows
        +all             ; catch all popups that start with an asterix
        +defaults)       ; default popup rules
       pretty-code       ; replace bits of code with pretty symbols
      ;tabbar            ; FIXME an (incomplete) tab bar for Emacs
       unicode           ; extended unicode support for various languages
       vc-gutter         ; vcs diff in the fringe
       vi-tilde-fringe   ; fringe tildes to mark beyond EOB
       window-select     ; visually switch windows

       :editor
      ;(format +onsave)  ; automated prettiness
      ;multiple-cursors  ; editing in many places at once
      ;parinfer          ; turn lisp into python, sort of
       rotate-text       ; cycle region at point between text candidates

       :emacs
       dired             ; making dired pretty [functional]
       ediff             ; comparing files in Emacs
       electric          ; smarter, keyword-based electric-indent
      ;eshell            ; a consistent, cross-platform shell (WIP)
       hideshow          ; basic code-folding support
       imenu             ; an imenu sidebar and searchable code index
      ;term              ; terminals in Emacs
       vc                ; version-control and Emacs, sitting in a tree

       :tools
       editorconfig      ; let someone else argue about tabs vs spaces
      ;ein               ; tame Jupyter notebooks with emacs
       gist              ; interacting with github gists
      ;macos             ; MacOS-specific commands
       make              ; run make tasks from Emacs
       magit             ;
       password-store    ; password manager for nerds
       pdf               ; pdf enhancements
      ;prodigy           ; FIXME managing external services & code builders
      ;rgb               ; creating color strings
      ;tmux              ; an API for interacting with tmux
      ;upload            ; map local to remote projects via ssh/ftp
      ;wakatime

       :lang
      ;assembly          ; assembly for fun or debugging
      ;(cc +irony +rtags); C/C++/Obj-C madness
       clojure           ; java with a lisp
      ;common-lisp       ; if you've seen one lisp, you've seen them all
      ;crystal           ; ruby at the speed of c
      ;csharp            ; unity, .NET, and mono shenanigans
       data              ; config/data formats
       erlang            ; an elegant language for a more civilized age
       elixir            ; erlang done right
      ;elm               ; care for a cup of TEA?
       emacs-lisp        ; drown in parentheses
      ;ess               ; emacs speaks statistics
      ;go                ; the hipster dialect
      ;(haskell +intero) ; a language that's lazier than I am
       haskell           ; a language that's lazier than I am
      ;hy                ; readability of scheme w/ speed of python
      ;(java +meghanada) ; the poster child for carpal tunnel syndrome
       javascript        ; all(hope(abandon(ye(who(enter(here))))))
      ;julia             ; a better, faster MATLAB
       latex             ; writing papers in Emacs has never been so fun
      ;ledger            ; an accounting system in Emacs
      ;lua               ; one-based indices? one-based indices
       markdown          ; writing docs for people to ignore
      ;nim               ; python + lisp at the speed of c
       nix               ; I hereby declare "nix geht mehr!"
      ;ocaml             ; an objective camel
       (org              ; organize your plain life in plain text
        +attach          ; custom attachment system
        +babel           ; running code in org
        +capture         ; org-capture in and outside of Emacs
        +export          ; Exporting org to whatever you want
        +present)        ; Emacs for presentations
      ;perl              ; write code no one else can comprehend
      ;php               ; perl's insecure younger brother
      ;plantuml          ; diagrams for confusing people more
      ;purescript        ; javascript, but functional
      ;python            ; beautiful is better than ugly
      ;qt                ; the 'cutest' gui framework ever
      ;racket            ; a DSL for DSLs
      ;rest              ; Emacs as a REST client
       ruby              ; 1.step do {|i| p "Ruby is #{i.even? ? 'love' : 'life'}"}
      ;rust              ; Fe2O3.unwrap().unwrap().unwrap().unwrap()
      ;scala             ; java, but good
       (sh +fish)        ; she sells (ba|z)sh shells on the C xor
      ;solidity          ; do you need a blockchain? No.
      ;swift             ; who asked for emoji variables?
       web               ; the tubes

       ;; Applications are complex and opinionated modules that transform Emacs
       ;; toward a specific purpose. They may have additional dependencies and
       ;; should be loaded late.
       :app
      ;(email +gmail)    ; emacs as an email client
       irc               ; how neckbeards socialize
      ;(rss +org)        ; emacs as an RSS reader
      ;twitter           ; twitter client https://twitter.com/vnought
      ;(write            ; emacs as a word processor (latex + org + markdown)
      ; +wordnut         ; wordnet (wn) search
      ; +langtool)       ; a proofreader (grammar/style check) for Emacs

       :collab
      ;floobits          ; peer programming for a price
      ;impatient-mode    ; show off code over HTTP

       :config
       ;; For literate config users. This will tangle+compile a config.org
       ;; literate config in your `doom-private-dir' whenever it changes.
      ;literate

       ;; The default module set reasonable defaults for Emacs. It also provides
       ;; a Spacemacs-inspired keybinding scheme, a custom yasnippet library,
       ;; and additional ex commands for evil-mode. Use it as a reference for
       ;; your own modules.
       (default +bindings +snippets +evil-commands))


;; I've swapped these keys on my keyboard
(setq x-super-keysym 'alt
      x-alt-keysym   'meta)

(setq user-mail-address "root@gws.fyi"
      user-full-name    "Griffin Smith")

; (def-package-hook! doom-themes :disable)

(after! rust
  (setq rust-format-on-save t))

; (defconst rust-src-path
;   (-> "/Users/griffin/.cargo/bin/rustc --print sysroot"
;       shell-command-to-string
;       string-trim
;       (concat "/lib/rustlib/src/rust/src")))
;
; (setenv "RUST_SRC_PATH" rust-src-path)
;
; (after! racer
;   (setq racer-rust-src-path rust-src-path))
;
(add-hook! rust-mode
  (flycheck-rust-setup)
  (flycheck-mode)
  (racer-mode)
  (cargo-minor-mode))

(add-hook! elixir-mode
  (require 'flycheck-credo)
  (setq flycheck-elixir-credo-strict t)
  (flycheck-credo-setup)

  (require 'flycheck-mix) (flycheck-mix-setup)

  (require 'flycheck-dialyxir) (flycheck-dialyxir-setup)

  (flycheck-mode))

(setq exec-path (append exec-path '("/Users/griffin/.cargo/bin")))

(after! cargo
  (setq cargo-process--custom-path-to-bin "/Users/griffin/.cargo/bin/cargo"))

(setq +solarized-s-base03    "#002b36"
      +solarized-s-base02    "#073642"
      ;; emphasized content
      +solarized-s-base01    "#586e75"
      ;; primary content
      +solarized-s-base00    "#657b83"
      +solarized-s-base0     "#839496"
      ;; comments
      +solarized-s-base1     "#93a1a1"
      ;; background highlight light
      +solarized-s-base2     "#eee8d5"
      ;; background light
      +solarized-s-base3     "#fdf6e3"

      ;; Solarized accented colors
      +solarized-yellow    "#b58900"
      +solarized-orange    "#cb4b16"
      +solarized-red       "#dc322f"
      +solarized-magenta   "#d33682"
      +solarized-violet    "#6c71c4"
      +solarized-blue      "#268bd2"
      +solarized-cyan      "#2aa198"
      +solarized-green     "#859900"

      ;; Darker and lighter accented colors
      ;; Only use these in exceptional circumstances!
      +solarized-yellow-d  "#7B6000"
      +solarized-yellow-l  "#DEB542"
      +solarized-orange-d  "#8B2C02"
      +solarized-orange-l  "#F2804F"
      +solarized-red-d     "#990A1B"
      +solarized-red-l     "#FF6E64"
      +solarized-magenta-d "#93115C"
      +solarized-magenta-l "#F771AC"
      +solarized-violet-d  "#3F4D91"
      +solarized-violet-l  "#9EA0E5"
      +solarized-blue-d    "#00629D"
      +solarized-blue-l    "#69B7F0"
      +solarized-cyan-d    "#00736F"
      +solarized-cyan-l    "#69CABF"
      +solarized-green-d   "#546E00"
      +solarized-green-l "#B4C342")

(defadvice load-theme (after theme-set-overrides activate)
  (dolist (theme-settings theme-overrides)
    (let ((theme (car theme-settings))
          (faces (cadr theme-settings)))
      (if (member theme custom-enabled-themes)
          (dolist (face faces)
            (custom-theme-set-faces theme face))))))

(defcustom theme-overrides nil
  "Association list of override faces to set for different custom themes.")

(defun alist-set (alist-symbol key value)
  "Set VALUE of a KEY in ALIST-SYMBOL."
  (set alist-symbol (cons (list key value) (assq-delete-all key (eval alist-symbol)))))

(alist-set 'theme-overrides 'grfn-solarized-light
           `((font-lock-doc-face ((t (:foreground ,+solarized-s-base1))))
             (font-lock-preprocessor-face ((t (:foreground ,+solarized-red))))
             (font-lock-keyword-face ((t (:foreground ,+solarized-green))))

             (elixir-attribute-face ((t (:foreground ,+solarized-blue))))
             (elixir-atom-face ((t (:foreground ,+solarized-cyan))))
             (linum ((t (:background ,+solarized-s-base2 :foreground ,+solarized-s-base1))))
             (line-number ((t (:background ,+solarized-s-base2 :foreground ,+solarized-s-base1))))

             (haskell-operator-face ((t (:foreground ,+solarized-green))))
             (haskell-keyword-face ((t (:foreground ,+solarized-cyan))))))

(add-to-list 'custom-theme-load-path "~/.doom.d/themes")
(load-theme 'grfn-solarized-light t)

(defface haskell-import-face `((t (:foreground ,+solarized-magenta))) "")

(setq doom-theme 'grfn-solarized-light)
; (setq doom-theme 'doom-solarized-light)

(add-hook! doom-post-init
  (set-face-attribute 'bold nil :weight 'ultra-light)
  (set-face-bold-p 'bold nil))

(defun rx-words (&rest words)
  (rx-to-string
   `(and symbol-start (group (or ,@words)) symbol-end)))

(font-lock-add-keywords
 'elixir-mode
 `((,(rx-words "def"
               "defp"
               "test"
               "describe"
               "property"
               "defrecord"
               "defmodule"
               "defstruct"
               "defdelegate"
               "defprotocol"
               "defimpl"
               "use"
               "import"
               "alias"
               "require"
               "assert"
               "refute"
               "assert_raise")
    .
    'font-lock-preprocessor-face)))

(font-lock-add-keywords
 'elixir-mode
 `((,(rx-words "def"
               "defp"
               "test"
               "describe"
               "property"
               "defrecord"
               "defmodule"
               "defstruct"
               "defdelegate"
               "use"
               "import"
               "alias"
               "require"
               "assert"
               "refute"
               "assert_raise")
    .
    'font-lock-preprocessor-face)))

(font-lock-add-keywords
 'haskell-mode
 `((,(rx-words "import") . 'haskell-import-face)))

;; (font-lock-add-keywords
;;  'haskell-mode
;;  `((,(rx "-- |") . 'haskell-keyword-face)))


(load-file (let ((coding-system-for-read 'utf-8))
                (shell-command-to-string "agda-mode locate")))
