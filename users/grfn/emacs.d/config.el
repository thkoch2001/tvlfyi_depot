;;; -*- lexical-binding: t; -*-

;; I've swapped these keys on my keyboard
(setq x-super-keysym 'alt
      x-alt-keysym   'meta)

(setq user-mail-address "root@gws.fyi"
      user-full-name    "Griffin Smith")

(let ((font-family (pcase system-type
                     ('darwin "MesloLGSDZ NF")
                     ('gnu/linux "Meslo LGSDZ Nerd Font"))))
  (setq doom-font (font-spec :family font-family :size 14)
        doom-big-font (font-spec :family font-family :size 24)
        doom-big-font-increment 5
        doom-variable-pitch-font (font-spec :family font-family)
        doom-unicode-font (font-spec :family font-family)))

(require 's)

(undefine-key! :keymaps 'doom-leader-map "/")

(load! "utils")
(load! "company-sql")
(load! "show-matching-paren")
(load! "irc")
(load! "github-org")
(load! "org-gcal")
(load! "grid")
(load! "nix")
(load! "email")
(load! "cpp")
(load! "lisp")
(load! "clojure")
(load! "rust")
(load! "terraform")

(require 'tvl)

(add-hook! elixir-mode
  (require 'flycheck-credo)
  (setq flycheck-elixir-credo-strict t)
  (flycheck-credo-setup)

  (require 'flycheck-mix) (flycheck-mix-setup)

  (require 'flycheck-dialyxir) (flycheck-dialyxir-setup)

  (flycheck-mode))

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

(defcustom theme-overrides nil
  "Association list of override faces to set for different custom themes.")

(defadvice load-theme (after theme-set-overrides activate)
  (dolist (theme-settings theme-overrides)
    (let ((theme (car theme-settings))
          (faces (cadr theme-settings)))
      (if (member theme custom-enabled-themes)
          (progn
            (dolist (face faces)
              (custom-theme-set-faces theme face)))))))

(defun alist-set (alist-symbol key value)
  "Set VALUE of a KEY in ALIST-SYMBOL."
  (set alist-symbol (cons (list key value) (assq-delete-all key (eval alist-symbol)))))

(comment
 (custom-theme-set-faces 'grfn-solarized-light
                         `(font-lock-doc-face
                           ((t (:foreground ,+solarized-s-base1)))))

+solarized-s-base1
(custom-theme-)
 (custom-face-get-current-spec 'font-lock-doc-face)

 )

(alist-set 'theme-overrides 'grfn-solarized-light
           `((font-lock-doc-face ((t (:foreground ,+solarized-s-base1))))
             (font-lock-preprocessor-face ((t (:foreground ,+solarized-red))))
             (font-lock-keyword-face ((t (:foreground ,+solarized-green :bold nil))))
             (font-lock-builtin-face ((t (:foreground ,+solarized-s-base01
                                                      :bold t))))

             (elixir-attribute-face ((t (:foreground ,+solarized-blue))))
             (elixir-atom-face ((t (:foreground ,+solarized-cyan))))
             (linum ((t (:background ,+solarized-s-base2 :foreground ,+solarized-s-base1))))
             (line-number ((t (:background ,+solarized-s-base2 :foreground ,+solarized-s-base1))))
             (line-number-current-line ((t (:background ,+solarized-s-base2 :foreground ,+solarized-s-base1))))

             (haskell-operator-face ((t (:foreground ,+solarized-green))))
             (haskell-keyword-face ((t (:foreground ,+solarized-cyan))))

             (org-drawer ((t (:foreground ,+solarized-s-base1
                              :bold t))))))

(setq solarized-use-variable-pitch nil
      solarized-scale-org-headlines nil
      solarized-use-less-bold t)

(add-to-list 'custom-theme-load-path "~/.doom.d/themes")
(load-theme 'grfn-solarized-light t)

(defface haskell-import-face `((t (:foreground ,+solarized-magenta))) "")

(setq doom-theme 'grfn-solarized-light)
; (setq doom-theme 'doom-solarized-light)

(add-hook! doom-post-init
  (set-face-attribute 'bold nil :weight 'ultra-light)
  (set-face-bold 'bold nil)
  (enable-theme 'grfn-solarized-light))

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


;; (load-file (let ((coding-system-for-read 'utf-8))
;;                 (shell-command-to-string "agda-mode locate")))

(defvar +grfn-dir (file-name-directory load-file-name))
(defvar +grfn-snippets-dir (expand-file-name "snippets/" +grfn-dir))

;;
(load! "+bindings")
(load! "+commands")
(load! "cpp")


(add-to-list 'load-path "/home/grfn/code/org-tracker")
(require 'org-tracker)
(use-package! org-tracker
  :hook (org-mode . org-tracker-mode)
  :config
  (setq org-tracker-state-alist '(("INBOX" . "Triage")
                                  ("BACKLOG" . "Backlog")
                                  ("TODO" . "Todo")
                                  ("ACTIVE" . "In Progress")
                                  ("PR" . "Code Review")
                                  ("DONE" . "Done")
                                  ("CANCELLED" . "Canceled"))
        org-tracker-username "griffin@readyset.io"
        org-tracker-claim-ticket-on-status-update '("ACTIVE" "PR" "DONE")
        org-tracker-create-stories-with-labels 'existing)

  (defun org-tracker-headlines-from-assigned-to-me (level)
    (interactive "*nLevel: ")
    (funcall-interactively
     #'org-tracker-headlines-from-search
     level
     "assignee = currentUser() and statusCategory = 2")))

(load! "+private")

(require 'dash)

(use-package! predd)


;;
;; Global config
;;

(setq doom-modeline-buffer-file-name-style 'relative-to-project
      doom-modeline-modal-icon nil
      doom-modeline-github t)

;;
;; Modules
;;

(after! smartparens
  ;; Auto-close more conservatively and expand braces on RET
  (let ((unless-list '(sp-point-before-word-p
                       sp-point-after-word-p
                       sp-point-before-same-p)))
    (sp-pair "'"  nil :unless unless-list)
    (sp-pair "\"" nil :unless unless-list))
  (sp-pair "{" nil :post-handlers '(("||\n[i]" "RET") ("| " " "))
           :unless '(sp-point-before-word-p sp-point-before-same-p))
  (sp-pair "(" nil :post-handlers '(("||\n[i]" "RET") ("| " " "))
           :unless '(sp-point-before-word-p sp-point-before-same-p))
  (sp-pair "[" nil :post-handlers '(("| " " "))
           :unless '(sp-point-before-word-p sp-point-before-same-p)))

;; feature/snippets
(after! yasnippet
  ;; Don't use default snippets, use mine.
  (setq yas-snippet-dirs
        (append (list '+grfn-snippets-dir)
                (delq 'yas-installed-snippets-dir yas-snippet-dirs))))

(after! company
  (setq company-idle-delay 0.2
        company-minimum-prefix-length 1))

(setq doom-modeline-height 12)



;; Should really figure out which of these is correct, eventually

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

(set-cursor-color +solarized-s-base02)

(after! doom-theme
  (set-face-foreground 'font-lock-doc-face +solarized-s-base1)
  (set-face-foreground 'org-block +solarized-s-base00)
  (set-face-foreground 'slack-message-output-header +solarized-s-base01)
  (set-face-attribute 'slack-message-output-header nil :underline nil)
  (set-face-attribute 'slack-message-output-text nil :height 1.0)
  )

(after! solarized-theme
  (set-face-foreground 'font-lock-doc-face +solarized-s-base1)
  (set-face-foreground 'org-block +solarized-s-base00)

  (set-face-foreground 'slack-message-output-header +solarized-s-base01)
  (set-face-attribute 'slack-message-output-header nil :underline nil)
  (set-face-attribute 'slack-message-output-text nil :height 1.0)
  )

(after! evil
  (setq evil-shift-width 2))

(after! org
  (load! "org-query")
  (load! "org-config"))

(after! magit
  (setq git-commit-summary-max-length 50))

(after! ivy
  ;; (setq ivy-re-builders-alist
  ;;       '((t . ivy--regex-fuzzy)))
  )

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(after! paxedit
  (add-hook! emacs-lisp-mode #'paxedit-mode)
  (add-hook! clojure-mode #'paxedit-mode)
  (add-hook! common-lisp-mode #'paxedit-mode))

(require 'haskell)

(let ((m-symbols
      '(("`mappend`" . "⊕")
        ("<>"        . "⊕")
        ("`elem`"   . "∈")
        ("`notElem`" . "∉"))))
  (dolist (item m-symbols) (add-to-list 'haskell-font-lock-symbols-alist item)))

(setq haskell-font-lock-symbols t)


(add-hook! haskell-mode
  ;; (intero-mode)
  (lsp-mode)
  ;; (flycheck-add-next-checker
  ;;  'intero
  ;;  'haskell-hlint)
  (set-fill-column 80)
  (setq evil-shift-width 2))

(auth-source-pass-enable)

(require 'fill-column-indicator)
;;; * Column Marker
(defun sanityinc/fci-enabled-p () (symbol-value 'fci-mode))

(defvar sanityinc/fci-mode-suppressed nil)
(make-variable-buffer-local 'sanityinc/fci-mode-suppressed)

(defadvice popup-create (before suppress-fci-mode activate)
  "Suspend fci-mode while popups are visible"
  (let ((fci-enabled (sanityinc/fci-enabled-p)))
    (when fci-enabled
      (setq sanityinc/fci-mode-suppressed fci-enabled)
      (turn-off-fci-mode))))

(defadvice popup-delete (after restore-fci-mode activate)
  "Restore fci-mode when all popups have closed"
  (when (and sanityinc/fci-mode-suppressed
             (null popup-instances))
    (setq sanityinc/fci-mode-suppressed nil)
    (turn-on-fci-mode)))


;;; Javascript

(require 'smartparens)

(setq js-indent-level 2)

(require 'prettier-js)
(after! prettier-js
  (add-hook! rjsx-mode #'prettier-js-mode)
  (add-hook! js2-mode  #'prettier-js-mode)
  (add-hook! json-mode #'prettier-js-mode)
  (add-hook! css-mode  #'prettier-js-mode))

(remove-hook 'js2-mode-hook 'tide-setup t)

;; Set this to the mode you use, I use rjsx-mode
(add-hook 'rjsx-mode-hook #'flow/set-flow-executable t)


;; Auto-format Haskell on save, with a combination of hindent + brittany

; (define-minor-mode brittany-haskell-mode
;   :init-value nil
;   :group 'haskell
;   :lighter "Brittany-Haskell"
;   :keymap '()
;   )


(require 'alert)
(setq alert-default-style 'libnotify)

;; (setq slack-buffer-function #'switch-to-buffer)

(setq projectile-test-suffix-function
      (lambda (project-type)
        (case project-type
          ('haskell-stack "Test")
          ('npm ".test")
          (otherwise (projectile-test-suffix project-type)))))

(setq projectile-create-missing-test-files 't)

(setq grfn/tracker-refs-re
      (rx line-start
          (or "Refs" "Fixes")
          ": "
          (one-or-more graph)
          line-end))

(defun grfn/add-tracker-reference-to-commit-message ()
  (interactive)
  (when-let* ((ticket-id (grfn/org-clocked-in-ticket-id)))
    (save-excursion
      (save-match-data
        (goto-char (point-min))
        ;; Don't add one if we've already got one
        (unless (search-forward-regexp grfn/tracker-refs-re nil t)
          (or
           (and
            (search-forward-regexp (rx line-start "Change-Id:") nil t)
            (forward-line -1))
           (and
            (search-forward-regexp (rx line-start "# Please enter") nil t)
            (forward-line -2)))
          (insert (format "\nRefs: %s" ticket-id)))))))

(defun grfn/switch-tracker-refs-fixes ()
  (interactive)
  (save-excursion
    (save-match-data
      (if (not (search-forward-regexp grfn/tracker-refs-re nil t))
          (message "Could not find reference to ticket")
        (goto-char (point-at-bol))
        (save-restriction
          (narrow-to-region (point-at-bol)
                            (point-at-eol))
          (or
           (and (search-forward "Refs" nil t)
                (replace-match "Fixes"))
           (and (search-forward "Fixes" nil t)
                (replace-match "Refs"))))))))

(after! magit
  (map! :map magit-mode-map
        ;; :n "] ]" #'magit-section-forward
        ;; :n "[ [" #'magit-section-backward
        )

  (transient-define-suffix magit-commit-wip ()
    (interactive)
    (magit-commit-create '("-m" "wip")))

  (transient-append-suffix
    #'magit-commit
    ["c"]
    (list "W" "Commit WIP" #'magit-commit-wip))

  (transient-define-suffix magit-reset-head-back ()
    (interactive)
    (magit-reset-mixed "HEAD~"))

  (transient-define-suffix magit-reset-head-previous ()
    (interactive)
    (magit-reset-mixed "HEAD@{1}"))

  (transient-append-suffix
    #'magit-reset
    ["f"]
    (list "b" "Reset HEAD~"    #'magit-reset-head-back))
  (transient-append-suffix
    #'magit-reset
    ["f"]
    (list "o" "Reset HEAD@{1}" #'magit-reset-head-previous))

  (defun magit-read-org-tracker-branch-name ()
    (when-let ((issue-id (org-tracker-clocked-in-issue-id)))
      (let ((desc
             (magit-read-string-ns
              (format "Issue description (to go after gs/%s/)"
                      issue-id))))
        (format "gs/%s/%s" issue-id desc))))

  (defun magit-read-org-tracker-branch-args ()
    (if-let ((issue-id (org-tracker-clocked-in-issue-id)))
        (let ((start-point (magit-read-starting-point
                            "Create and checkout branch for Tracker issue"
                            nil
                            "origin/master")))
          (if (magit-rev-verify start-point)
              (when-let ((desc (magit-read-org-tracker-branch-name)))
                (list desc start-point))
            (user-error "Not a valid starting point: %s" choice)))
      (user-error "No currently clocked-in tracker issue")))

  (transient-define-suffix magit-checkout-org-tracker-branch (branch start-point)
    (interactive (magit-read-org-tracker-branch-args))
    (magit-branch-and-checkout branch start-point))

  (transient-define-suffix magit-rename-org-tracker-branch (old new)
    (interactive
     (let ((branch (magit-read-local-branch "Rename branch")))
       (list branch (magit-read-org-tracker-branch-name))))
    (when (and old new)
      (magit-branch-rename old new)))

  (transient-append-suffix
    #'magit-branch
    ["c"]
    (list "C" "Checkout Tracker branch" #'magit-checkout-org-tracker-branch))
  (transient-append-suffix
    #'magit-branch
    ["c"]
    (list "M" "Rename branch to Tracker ticket" #'magit-rename-org-tracker-branch))

  )

(add-hook 'git-commit-setup-hook #'grfn/add-tracker-reference-to-commit-message)
(map! (:map git-commit-mode-map
       "C-c C-f" #'grfn/switch-tracker-refs-fixes))

;; (defun grfn/split-window-more-sensibly (&optional window)
;;   (let ((window (or window (selected-window))))
;;     (or (and (window-splittable-p window)
;;              ;; Split window vertically.
;;              (with-selected-window window
;;                (split-window-right)))
;;         (and (window-splittable-p window t)
;;              ;; Split window horizontally.
;;              (with-selected-window window
;;                (split-window-right)))
;;         (and (eq window (frame-root-window (window-frame window)))
;;              (not (window-minibuffer-p window))
;;              ;; If WINDOW is the only window on its frame and is not the
;;              ;; minibuffer window, try to split it vertically disregarding
;;              ;; the value of `split-height-threshold'.
;;              (let ((split-height-threshold 0))
;;                (when (window-splittable-p window)
;;                  (with-selected-window window
;;                    (split-window-below))))))))

(use-package! lsp-mode
  :after (:any haskell-mode)
  :config
  (setq lsp-response-timeout 60)
  :hook
  (haskell-mode . lsp-mode))

(use-package! lsp-ui
  :after lsp-mode
  :config
  (defun +grfn/lsp-ui-doc-frame-hook (frame window)
    (set-frame-font (if doom-big-font-mode doom-big-font doom-font)
                    nil (list frame)))
  (setq lsp-ui-flycheck-enable t
        lsp-ui-doc-header nil
        lsp-ui-doc-position 'top
        lsp-ui-doc-alignment 'window
        lsp-ui-doc-frame-hook '+grfn/lsp-ui-doc-frame-hook
        lsp-ui-doc-max-width 150
        lsp-ui-doc-max-height 13)
  (setq imenu-auto-rescan t)
  (set-face-background 'lsp-ui-doc-background +solarized-s-base2)
  (set-face-background 'lsp-face-highlight-read +solarized-s-base2)
  (set-face-background 'lsp-face-highlight-write +solarized-s-base2)
  :hook
  (lsp-mode . lsp-ui-mode)
  (lsp-ui-mode . flycheck-mode))

(use-package! company-lsp
  :after (lsp-mode lsp-ui)
  :config
  (add-to-list #'company-backends #'company-lsp)
  (setq company-lsp-async t))

(defun +grfn/haskell-mode-setup ()
  (interactive)
  (flymake-mode -1)
  (add-to-list 'flycheck-disabled-checkers 'haskell-ghc)

  (flycheck-remove-next-checker 'lsp 'haskell-ghc)
  (flycheck-add-next-checker 'lsp '(warning . haskell-hlint))

  ;; If there’s a 'hie.sh' defined locally by a project
  ;; (e.g. to run HIE in a nix-shell), use it…
  (when-let ((project-dir (locate-dominating-file default-directory "hie.sh")))
    (cl-flet
        ((which (cmd)
                (s-trim
                 (shell-command-to-string
                  (concat
                   "nix-shell "
                   (expand-file-name "shell.nix" project-dir)
                   " --run \"which " cmd "\" 2>/dev/null")))))
      (setq-local
       lsp-haskell-process-path-hie (expand-file-name "hie.sh" project-dir)
       haskell-hoogle-command (which "hoogle"))))
  ;; … and only then setup the LSP.
  (lsp))

(defun never-flymake-mode (orig &rest args)
  (when (and (bound-and-true-p flymake-mode))
    (funcall orig 0)
    (message "disabled flymake-mode")))
(advice-add #'flymake-mode :around #'never-flymake-mode)

(defun +grfn/wrap-lsp-haskell-process (argv)
  (let* ((project-dir (locate-dominating-file
                       (buffer-file-name)
                       "hie.yaml"))
         (shell-dot-nix (expand-file-name "shell.nix" project-dir)))
    ;; (when (string-equal default-directory "/home/grfn/code/depot")
    ;;   (debug))
    (message "%s %s %s %s"
             (buffer-file-name)
             default-directory
             project-dir
             shell-dot-nix)
    (if (file-exists-p shell-dot-nix)
        `("bash" "-c"
          ,(format "cd %s && nix-shell %s --run '%s'"
                   project-dir
                   shell-dot-nix
                   (s-join " " argv)))
      argv)))

(use-package! lsp-haskell
  :after (lsp-mode lsp-ui haskell-mode)
  ;; :hook
  ;; (haskell-mode . lsp-haskell-enable)
  :config
  (setq lsp-haskell-process-path-hie "haskell-language-server-wrapper"
        lsp-haskell-process-args-hie
        '("-d" "-l" "/tmp/hie.log" "+RTS" "-M4G" "-H1G" "-K4G" "-A16M" "-RTS")
        lsp-haskell-process-wrapper-function
        #'+grfn/wrap-lsp-haskell-process)
  (add-hook 'haskell-mode-hook #'+grfn/haskell-mode-setup 't))

(use-package! lsp-imenu
  :after (lsp-mode lsp-ui)
  :hook
  (lsp-after-open . lsp-enable-imenu))

;; (use-package! counsel-etags
;;   :ensure t
;;   :init
;;   (add-hook 'haskell-mode-hook
;;             (lambda ()
;;               (add-hook 'after-save-hook
;;                         'counsel-etags-virtual-update-tags 'append 'local)))
;;   :config
;;   (setq counsel-etags-update-interval 60)
;;   ;; (push "build" counsel-etags-ignore-directories)
;;   )

;; (use-package! evil-magit
;;   :after (magit))

(use-package! writeroom-mode)

(use-package! graphql-mode)


(require 'whitespace)
(setq whitespace-style '(face lines-tail))
(global-whitespace-mode t)
(add-hook 'org-mode-hook (lambda ()  (whitespace-mode -1)) t)

(set-face-foreground 'whitespace-line +solarized-red)
(set-face-attribute 'whitespace-line nil :underline 't)

;; (set-face-background 'ivy-posframe +solarized-s-base3)
;; (set-face-foreground 'ivy-posframe +solarized-s-base01)

(let ((base03    "#002b36")
      (base02    "#073642")
      (base01    "#586e75")
      (base00    "#657b83")
      (base0     "#839496")
      (base1     "#93a1a1")
      (base2     "#eee8d5")
      (base3     "#fdf6e3")
      (yellow    "#b58900")
      (orange    "#cb4b16")
      (red       "#dc322f")
      (magenta   "#d33682")
      (violet    "#6c71c4")
      (blue      "#268bd2")
      (cyan      "#2aa198")
      (green     "#859900"))
  (custom-set-faces
   `(agda2-highlight-keyword-face ((t (:foreground ,green))))
   `(agda2-highlight-string-face ((t (:foreground ,cyan))))
   `(agda2-highlight-number-face ((t (:foreground ,violet))))
   `(agda2-highlight-symbol-face ((((background ,base3)) (:foreground ,base01))))
   `(agda2-highlight-primitive-type-face ((t (:foreground ,blue))))
   `(agda2-highlight-bound-variable-face ((t nil)))
   `(agda2-highlight-inductive-constructor-face ((t (:foreground ,green))))
   `(agda2-highlight-coinductive-constructor-face ((t (:foreground ,yellow))))
   `(agda2-highlight-datatype-face ((t (:foreground ,blue))))
   `(agda2-highlight-field-face ((t (:foreground ,red))))
   `(agda2-highlight-function-face ((t (:foreground ,blue))))
   `(agda2-highlight-module-face ((t (:foreground ,yellow))))
   `(agda2-highlight-postulate-face ((t (:foreground ,blue))))
   `(agda2-highlight-primitive-face ((t (:foreground ,blue))))
   `(agda2-highlight-record-face ((t (:foreground ,blue))))
   `(agda2-highlight-dotted-face ((t nil)))
   `(agda2-highlight-operator-face ((t nil)))
   `(agda2-highlight-error-face ((t (:foreground ,red :underline t))))
   `(agda2-highlight-unsolved-meta-face ((t (:background ,base2))))
   `(agda2-highlight-unsolved-constraint-face ((t (:background ,base2))))
   `(agda2-highlight-termination-problem-face ((t (:background ,orange :foreground ,base03))))
   `(agda2-highlight-incomplete-pattern-face ((t (:background ,orange :foreground ,base03))))
   `(agda2-highlight-typechecks-face ((t (:background ,cyan :foreground ,base03))))))


(after! cider
  (setq cider-prompt-for-symbol nil
        cider-font-lock-dynamically 't
        cider-save-file-on-load 't)
  )

(comment
 (setq elt (+org-clocked-in-element))

 (eq 'headline (car elt))
 (plist-get (cadr elt) :raw-value)
 )

(defun +org-headline-title (headline)
  (when (eq 'headline (car elt))
    (plist-get (cadr elt) :raw-value)))

;; (setq +ligatures-extra-symbols
;;       (append +ligatures-extra-symbols
;;               '(:equal     "≡"
;;                 :not-equal "≠"
;;                 :is        "≣"
;;                 :isnt      "≢"
;;                 :lte       "≤"
;;                 :gte       "≥"
;;                 :subseteq  "⊆"
;;                 )))

;; (after! python
;;   (set-pretty-symbols! 'python-mode :merge t
;;     :equal      "=="
;;     :not-equal "!="
;;     :lte "<="
;;     :gte ">="
;;     :is  "is"
;;     :isnt "is not"
;;     :subseteq "issubset"

;;     ;; doom builtins

;;     ;; Functional
;;     :def "def"
;;     :lambda "lambda"
;;     ;; Types
;;     :null "None"
;;     :true "True" :false "False"
;;     :int "int" :str "str"
;;     :float "float"
;;     :bool "bool"
;;     :tuple "tuple"
;;     ;; Flow
;;     :not "not"
;;     :in "in" :not-in "not in"
;;     :and "and" :or "or"
;;     :for "for"
;;     :return "return" :yield "yield"))

(use-package! sqlup-mode
  :hook
  (sql-mode-hook . sqlup-mode)
  (sql-interactive-mode-hook . sqlup-mode))

(use-package! emacsql)
(use-package! emacsql-psql
  :after (emacsql))

(use-package! pyimport
  :after (python))

(use-package! blacken
  :after (python)
  :init
  (add-hook #'python-mode-hook #'blacken-mode)
  :config
  (setq blacken-only-if-project-is-blackened t
        blacken-allow-py36 t
        blacken-line-length 100))

(after! python
  (defun +python-setup ()
    (setq-local fill-column 100
                whitespace-line-column 100
                flycheck-disabled-checkers '(python-flake8)
                flycheck-checker 'python-pylint))

  (add-hook #'python-mode-hook #'+python-setup)
  (add-hook #'python-mode-hook #'lsp)
  (remove-hook #'python-mode-hook #'pipenv-mode))

; (use-package! w3m
;   :config
;   (setq browse-url-browser-function
;         `(("^https://app.clubhouse.io.*" . browse-url-firefox)
;           ("^https://github.com.*" . browse-url-firefox)
;           (".*" . browse-url-firefox))))

(use-package! ob-http
  :config
  (add-to-list 'org-babel-load-languages '(http . t)))

;; (use-package! ob-ipython
;;   :after (pyimport)
;;   :config
;;   (add-to-list 'org-babel-load-languages '(ipython . t))
;;   (setq ob-ipython-command
        ;; "/home/griffin/code/urb/ciml-video-classifier/bin/jupyter"))

(use-package! counsel-spotify)

(after! counsel
  (map! [remap counsel-org-capture] #'org-capture
        [remap org-capture] #'org-capture))

(remove-hook 'doom-first-input-hook #'evil-snipe-mode)

(use-package! rainbow-mode)

(use-package! org-alert
  :disabled t
  :config
  (org-alert-enable)
  (setq alert-default-style 'libnotify
        org-alert-headline-title "org"))

(use-package! ob-async)

(use-package! org-recent-headings
  :config
  (map! :n "SPC n r" #'org-recent-headings-ivy))

(use-package! org-sticky-header
  :after (org)
  :hook (org-mode-hook . org-sticky-header-mode)
  :config
  (setq-default org-sticky-header-heading-star "●"))

(enable-theme 'grfn-solarized-light)

;;; this needs to be *after the theme*, or else I get no agenda items.
;;; whuuu??
(load! "org-config")


;;; word-char
(add-hook! prog-mode
  (modify-syntax-entry ?_ "w"))

(add-hook! lisp-mode
  (modify-syntax-entry ?- "w"))

(after! flycheck
  (put 'flycheck-python-pylint-executable 'safe-local-variable (lambda (_) t))
  (setq flycheck-error-list-minimum-level 'warn
        flycheck-navigation-minimum-level 'warn))

(defvar alembic-command "alembic"
  "Command to execute when running alembic")

(defvar alembic-dir-fun (lambda () default-directory)
  "Reference to a function whose return value will be used as the directory to
  run Alembic in")

(put 'alembic-command 'safe-local-variable (lambda (_) t))
(put 'alembic-dir-fun 'safe-local-variable (lambda (_) t))

(defun make-alembic-command (args)
  (if (functionp alembic-command)
      (funcall alembic-command args)
    (concat alembic-command " " args)))

(defun +grfn/extract-alembic-migration-name (output)
  (unless (string-match (rx (0+ anything) "Generating "
                            (group (one-or-more (not (syntax whitespace))))
                            " ..." (one-or-more (syntax whitespace)) "done"
                            (0+ anything))
                        output)
    (user-error "Error: %s" output))
  (match-string-no-properties 1 output))

(defun -run-alembic (args)
  (let* ((default-directory (funcall alembic-dir-fun))
         (command (make-alembic-command args))
         ;; (format "nix-shell --run 'alembic %s'" args)
         ;; (format "%s %s" alembic-command args)
         (res
          (with-temp-buffer
            (cons
             (shell-command command t)
             (s-replace-regexp
              "^.*Nix search path entry.*$" ""
              (buffer-string)))))
         (exit-code (car res))
         (out (cdr res)))
    ;; (if (= 0 exit-code)
    ;;     out
    ;;   (error "Error running %s: %s" command out))
    out
    ))

(comment
 --exit-code
 --bs
 )

(defun run-alembic (args)
  (interactive "sAlembic command: ")
  (message "%s" (-run-alembic args)))

(defun generate-alembic-migration (msg &rest args)
  (interactive "sMessage: ")
  (->
   (format "revision %s -m \"%s\""
           (s-join " " args)
           msg)
   (-run-alembic)
   (+grfn/extract-alembic-migration-name)
   (find-file-other-window)))

(cl-defun alembic-upgrade (&optional revision &key namespace)
  (interactive "sRevision: ")
  (let ((default-directory (funcall alembic-dir-fun)))
    (run-alembic (format "%s upgrade %s"
                         (if namespace (concat "-n " namespace) "")
                         (or revision "head")))))

(defun alembic-downgrade (revision)
  (interactive "sRevision: ")
  (let ((default-directory (funcall alembic-dir-fun)))
    (run-alembic (format "downgrade %s" (or revision "head")))))

(use-package! gnuplot)
(use-package! gnuplot-mode :after gnuplot)
(use-package! string-inflection)

(after! anaconda-mode
  ;; (set-company-backend! 'anaconda-mode #'company-yasnippet)
  )

;; (add-hook! python-mode
;;   (capf))

(cl-defstruct pull-request url number title author repository)

(defun grfn/num-inbox-items ()
  (length (org-elements-agenda-match "inbox" t)))

(use-package! dhall-mode
  :mode "\\.dhall\\'")

(use-package! github-review
  :after forge)

(after! forge
  (set-popup-rule!
    "^\\*forge"
    :size 0.75))

(defun grfn/org-add-db-connection-params ()
  (interactive)
  (ivy-read
   "DB to connect to: "
   (-map (lambda (opts)
           (propertize (symbol-name (car opts))
                       'header-args (cdr opts)))
         db-connection-param-options)
   :require-match t
   :action
   (lambda (opt)
     (let ((header-args (get-text-property 0 'header-args opt)))
       (org-set-property "header-args" header-args)))))

(use-package! kubernetes
  :commands (kubernetes-overview))

(use-package! k8s-mode
  :hook (k8s-mode . yas-minor-mode))

(use-package! sx)

;; (use-package! nix-update
;;   :config
;;   (map! (:map nix-mode-map
;;           (:leader
;;             :desc "Update fetcher" :nv #'nix-update-fetch))))


(after! lsp-haskell
  (lsp-register-client
   (make-lsp--client
    :new-connection (lsp-stdio-connection (lambda () (lsp-haskell--hie-command)))
    :major-modes '(haskell-mode)
    :server-id 'hie
    ;; :multi-root t
    ;; :initialization-options 'lsp-haskell--make-init-options
    )
   )
  )

(solaire-global-mode -1)

(use-package! wsd-mode)

(use-package! metal-mercury-mode)
(use-package! flycheck-mercury
  :after (metal-mercury-mode flycheck-mercury))

(use-package! direnv
  :config (direnv-mode))

(after! erc
  ;; (setq erc-autojoin-channels-alist '(("freenode.net" "#nixos" "#haskell" "##tvl")))
  )

(defun evil-disable-insert-state-bindings ()
  evil-disable-insert-state-bindings)

;; (use-package! terraform-mode)
;; (use-package! company-terraform
;;   :after terraform-mode
;;   :config (company-terraform-init))

(use-package! znc
  :config
  (setq znc-servers
        '(("znc.gws.fyi" 5000 t
           ((freenode "glittershark" "Ompquy"))))))

(use-package! jsonnet-mode
  :config
  (map!
   (:map jsonnet-mode-map
    (:n "g SPC" #'jsonnet-eval-buffer))))

(add-to-list 'safe-local-variable-values
             '(truncate-lines . t))

(set-popup-rule!
  "^\\*gud-"
  :quit nil)

(setq elcord-editor-icon "emacs_icon")
