;;; private/grfn/config.el -*- lexical-binding: t; -*-

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

(setq solarized-use-variable-pitch nil
      solarized-scale-org-headlines nil)

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

(defvar +grfn-dir (file-name-directory load-file-name))
(defvar +grfn-snippets-dir (expand-file-name "snippets/" +grfn-dir))

;;
(when (featurep! :feature evil)
  (load! "+bindings")
  (load! "+commands"))

(load! "+private")

(require 'dash)


;;
;; Global config
;;

(setq +doom-modeline-buffer-file-name-style 'relative-to-project)

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

;; feature/evil
(after! evil-mc
  ;; Make evil-mc resume its cursors when I switch to insert mode
  (add-hook! 'evil-mc-before-cursors-created
    (add-hook 'evil-insert-state-entry-hook #'evil-mc-resume-cursors nil t))
  (add-hook! 'evil-mc-after-cursors-deleted
    (remove-hook 'evil-insert-state-entry-hook #'evil-mc-resume-cursors t)))

;; feature/snippets
(after! yasnippet
  ;; Don't use default snippets, use mine.
  (setq yas-snippet-dirs
        (append (list '+grfn-snippets-dir)
                (delq 'yas-installed-snippets-dir yas-snippet-dirs))))

(after! company
  (setq company-idle-delay 0.2
        company-minimum-prefix-length 1))

(setq +doom-modeline-height 10)

;; lang/org
;; (after! org-bullets
;;   ;; The standard unicode characters are usually misaligned depending on the
;;   ;; font. This bugs me. Personally, markdown #-marks for headlines are more
;;   ;; elegant, so we use those.
;;   (setq org-bullets-bullet-list '("#")))

;; (defmacro faces! (mode &rest forms)
;;   (let ((hook-name (-> mode symbol-name (concat "-hook"))))
;;     (if-let ((hook-sym (intern-soft hook-name)))
;;         `(add-hook! ,hook-sym
;;            (message "HELLO I AM MACRO TIME")
;;            ,@(->
;;               forms
;;               (seq-partition 2)
;;               (->> (seq-map
;;                     (lambda (pair)
;;                       (let ((face  (car pair))
;;                             (color (cadr pair)))
;;                         `(set-face-foreground ,face ,color)))))))
;;       (warn "Hook name %s (for mode %s) does not exist as symbol!"
;;             (hook-name)
;;             (symbol-name mode)))))

(def-package! org-clubhouse)

; (require 'doom-themes)

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

(after! slack
  (set-face-foreground 'slack-message-output-header +solarized-s-base01)
  (set-face-attribute 'slack-message-output-header nil :underline nil)
  (set-face-attribute 'slack-message-output-text nil :height 1.0))

(after! evil
  (setq evil-shift-width 2))

(after! org
  (setq
   org-directory (expand-file-name "~/notes")
   +org-dir (expand-file-name "~/notes")
   org-default-notes-file (concat org-directory "/inbox.org")
   +org-default-todo-file (concat org-directory "/inbox.org")
   org-agenda-files (list (expand-file-name "~/notes"))
   org-refile-targets '((org-agenda-files :maxlevel . 1))
   org-file-apps `((auto-mode . emacs)
                   (,(rx (or (and "." (optional "x") (optional "htm") (optional "l") buffer-end)
                             (and buffer-start "http" (optional "s") "://")))
                    . "firefox %s")
                   (,(rx ".pdf" buffer-end) . "apvlv %s")
                   (,(rx "." (or "png"
                                 "jpg"
                                 "jpeg"
                                 "gif"
                                 "tif"
                                 "tiff")
                         buffer-end)
                    . "feh %s"))
   org-log-done 'time
   org-archive-location "~/notes/trash::* From %s"
   org-cycle-separator-lines 2
   org-hidden-keywords '(title)
   org-tags-column -130
   org-ellipsis "⤵"
   org-capture-templates
   `(("t" "Todo" entry
      (file+headline +org-default-todo-file "Inbox")
      "* TODO %?\n%i" :prepend t :kill-buffer t)

     ("n" "Notes" entry
      (file+headline +org-default-notes-file "Inbox")
      "* %u %?\n%i" :prepend t :kill-buffer t))
   org-deadline-warning-days 1
   org-agenda-skip-scheduled-if-deadline-is-shown 'todo
   org-agenda-custom-commands
   '(("p" "Sprint Tasks" tags-todo "sprint")
     ("i" "Inbox" tags "inbox")))
  (set-face-foreground 'org-block +solarized-s-base00)
  (add-hook! org-mode
    (add-hook! evil-normal-state-entry-hook
      #'org-align-all-tags))
  (setf (alist-get 'file org-link-frame-setup) 'find-file-other-window)
  (set-face-foreground 'org-block +solarized-s-base00)
  )

(after! magit
  (setq git-commit-summary-max-length 50)
  (require 'magit-gh-pulls)
  (add-hook 'magit-mode-hook 'turn-on-magit-gh-pulls))

(comment

 (string-match-p "(?!foo).*" "bar")
 )

(after! ivy
  (setq ivy-re-builders-alist
        '((t . ivy--regex-fuzzy))))

(setq doom-font (font-spec :family "Meslo LGSDZ Nerd Font" :size 14)
      doom-big-font (font-spec :family "Meslo LGSDZ Nerd Font" :size 19)
      doom-variable-pitch-font (font-spec :family "DejaVu Sans")
      doom-unicode-font (font-spec :family "Meslo LG S DZ"))

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(after! paxedit
  (add-hook! emacs-lisp-mode #'paxedit-mode)
  (add-hook! clojure-mode #'paxedit-mode))

(require 'haskell)

(let ((m-symbols
      '(("`mappend`" . "⊕")
        ("<>"        . "⊕")
        ("`elem`"   . "∈")
        ("`notElem`" . "∉"))))
  (dolist (item m-symbols) (add-to-list 'haskell-font-lock-symbols-alist item)))

(setq haskell-font-lock-symbols t)


(add-hook! haskell-mode
  (intero-mode)
  ;; (lsp-mode)
  (flycheck-add-next-checker
   'intero
   'haskell-hlint)
  (set-fill-column 80)
  (setq evil-shift-width 2))

;; (load! org-clubhouse)
(add-hook! org-mode #'org-clubhouse-mode)

(load! "slack-snippets")

(after! magit
  (require 'evil-magit)
  ;; (require 'magithub)
  )

; (require 'auth-password-store)
; (auth-pass-enable)
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


;; https://github.com/alpaker/Fill-Column-Indicator/issues/67#issuecomment-195611974
;; (add-hook 'prog-mode-hook #'fci-mode)
;; (after! fill-column-indicator
;;   (add-hook 'prog-mode-hook #'fci-mode)
;;   (defvar eos/fci-disabled nil)
;;   (make-variable-buffer-local 'eos/fci-disabled)

;;   ;; Add a hook that disables fci if enabled when the window changes and it
;;   ;; isn't wide enough to display it.
;;   (defun eos/maybe-disable-fci ()
;;     (interactive)
;;     ;; Disable FCI if necessary
;;     (when (and fci-mode
;;                (< (window-width) (or fci-rule-column fill-column)))
;;       (fci-mode -1)
;;       (setq-local eos/fci-disabled t))
;;     ;; Enable FCI if necessary
;;     (when (and eos/fci-disabled
;;                (eq fci-mode nil)
;;                (> (window-width) (or fci-rule-column fill-column)))
;;       (fci-mode 1)
;;       (setq-local eos/fci-disabled nil)))

;;   (defun eos/add-fci-disabling-hook ()
;;     (interactive)
;;     (add-hook 'window-configuration-change-hook
;;               #'eos/maybe-disable-fci))

;;   (add-hook 'prog-mode-hook #'eos/add-fci-disabling-hook))


;;; Javascript

(require 'smartparens)

(setq js-indent-level 2)

(require 'prettier-js)
(after! prettier-js
  (add-hook! rjsx-mode #'prettier-js-mode)
  (add-hook! js2-mode  #'prettier-js-mode)
  (add-hook! json-mode #'prettier-js-mode)
  (add-hook! css-mode  #'prettier-js-mode))

(require 'flycheck-flow)
(with-eval-after-load 'flycheck
  (flycheck-add-mode 'javascript-flow 'rjsx-mode)
  (flycheck-add-mode 'javascript-flow 'flow-minor-mode)
  (flycheck-add-mode 'javascript-eslint 'flow-minor-mode)
  (flycheck-add-next-checker 'javascript-flow 'javascript-eslint))


(require 'flow-minor-mode)

(remove-hook 'js2-mode-hook 'tide-setup t)

(require 'company-flow)
(eval-after-load 'company
  (lambda () (add-to-list 'company-backends 'company-flow)))
(defun flow/set-flow-executable ()
  (interactive)
  (let* ((os (pcase system-type
               ('darwin "osx")
               ('gnu/linux "linux64")
               (_ nil)))
         (root (locate-dominating-file  buffer-file-name  "node_modules/flow-bin"))
         (executable (car (file-expand-wildcards
                           (concat root "node_modules/flow-bin/*" os "*/flow")))))
    (setq-local company-flow-executable executable)
    ;; These are not necessary for this package, but a good idea if you use
    ;; these other packages
    (setq-local flow-minor-default-binary executable)
    (setq-local flycheck-javascript-flow-executable executable)))

;; Set this to the mode you use, I use rjsx-mode
(add-hook 'rjsx-mode-hook #'flow/set-flow-executable t)


;; Auto-format Haskell on save, with a combination of hindent + brittany

(define-minor-mode brittany-haskell-mode
  :init-value nil
  :group 'haskell
  :lighter "Brittany-Haskell"
  :keymap '()
  )


(defun urbint/format-haskell-source ()
  (interactive)
  (let ((output-buffer (generate-new-buffer "brittany-out"))
        (config-file-path
         (concat (string-trim
                  (shell-command-to-string "stack path --project-root"))
                 "/brittany.yaml")))
    (when (= 0 (call-process-region
                (point-min) (point-max)
                "stack"
                nil output-buffer nil
                "exec" "--" "brittany" "--config-file" config-file-path))
      (let ((pt (point))
            (wst (window-start))
            (formatted-source (with-current-buffer output-buffer
                                (buffer-string))))
        (erase-buffer)
        (insert formatted-source)
        (goto-char pt)
        (set-window-start nil wst)))))

(add-hook
 'before-save-hook
 (lambda ()
   (when (and (eq major-mode 'haskell-mode)
              (bound-and-true-p brittany-haskell-mode))
     (urbint/format-haskell-source))))

(require 'slack)
(setq slack-buffer-emojify 't
      slack-prefer-current-team 't)
(require 'alert)
(setq alert-default-style 'libnotify)

;; (setq slack-buffer-function #'switch-to-buffer)

(setq projectile-test-suffix-function
      (lambda (project-type)
        (case project-type
          ('haskell-stack "Test")
          ('npm ".test")
          (otherwise (projectile-test-suffix project-type)))))

(defun magit-commit-wip ()
  (interactive)
  (magit-commit '("-m" "wip")))

(after! magit
  (magit-define-popup-action 'magit-commit-popup
    ?W "WIP" 'magit-commit-wip))

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

;; (def-package! lsp-mode
;;   :after (:any haskell-mode)
;;   :config
;;   (lsp-mode)
;;   (setq lsp-project-whitelist '("^/home/griffin/code/urb/grid/$")
;;         lsp-response-timeout 60)
;;   :hook
;;   (haskell-mode . lsp-mode))

;; (def-package! lsp-ui
;;   :after lsp-mode
;;   :config
;;   (setq lsp-ui-flycheck-enable t)
;;   (setq imenu-auto-rescan t)
;;   (set-face-background 'lsp-ui-doc-background +solarized-s-base2)
;;   (set-face-background 'lsp-face-highlight-read +solarized-s-base2)
;;   (set-face-background 'lsp-face-highlight-write +solarized-s-base2)
;;   :hook
;;   (lsp-mode . lsp-ui-mode)
;;   (lsp-ui-mode . flycheck-mode))

;; (def-package! company-lsp
;;   :after (lsp-mode lsp-ui)
;;   :config
;;   (setq company-backends '(company-lsp))
;;   (setq company-lsp-async t))

;; (def-package! lsp-haskell
;;   :after (lsp-mode lsp-ui haskell-mode)
;;   :hook
;;   (haskell-mode . lsp-haskell-enable)
;;   :config
;;   (setq lsp-haskell-process-path-hie "/home/griffin/.local/bin/hie-wrapper"
;;         lsp-haskell-process-args-hie
;;           '("-d" "-l" "/tmp/hie.log" "+RTS" "-M4G" "-H1G" "-K4G" "-A16M" "-RTS"
;;             "--lsp")))

;; (def-package! lsp-imenu
;;   :after (lsp-mode lsp-ui)
;;   :hook
;;   (lsp-after-open . lsp-enable-imenu))

(def-package! evil-magit
  :after (magit))

(def-package! writeroom-mode)

(def-package! graphql-mode)

(require 'whitespace)
(setq whitespace-style '(face lines-tail))
(global-whitespace-mode t)
(add-hook! 'org-mode-hook (lambda () (whitespace-mode -1)))

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
