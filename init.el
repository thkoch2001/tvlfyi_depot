(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/") t)
(package-initialize)


;; TODO: macro definitions
;; (defmacro add-hooks (hooks callback)
;;   "Convenience macro for creating multiple hooks."
;;   (mapcar (lambda (mode) `(add-hook ,mode ,callback)) hooks))


;; use-package configuration
(require 'use-package)
(setq use-package-always-ensure t)


;; disable custom variable entries from being written to ~/.emacs.d/init.el
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)


;; transparently edit compressed files
(auto-compression-mode t)


;; disable line-wrapping
(setq-default truncate-lines 1)


;; change font
(add-to-list 'default-frame-alist '(font . "Operator Mono"))


;; create file bookmarks
(set-register ?e '(file . "~/.emacs.d/init.el"))
(set-register ?u '(file . "~/urbint"))
(set-register ?d '(file . "~/Dropbox"))


;; change emacs prompts from "yes or no" -> "y or n"
(fset 'yes-or-no-p 'y-or-n-p)


;; function to copy current file buffer path to clipboard
(defun my-put-file-name-on-clipboard ()
  "Put the current file name on the clipboard"
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (with-temp-buffer
        (insert filename)
        (clipboard-kill-region (point-min) (point-max)))
      (message filename))))


;; open photos in Emacs
(auto-image-file-mode 1)


;; working with remote files
(require 'tramp)
(setq tramp-default-method "ssh")


;; persist history etc b/w Emacs sessions
(setq desktop-save 'if-exists)
(desktop-save-mode 1)

(setq desktop-globals-to-save
      (append '((extended-command-history . 30)
                (file-name-history        . 100)
                (grep-history             . 30)
                (compile-history          . 30)
                (minibuffer-history       . 50)
                (query-replace-history    . 60)
                (read-expression-history  . 60)
                (regexp-history           . 60)
                (regexp-search-ring       . 20)
                (search-ring              . 20)
                (shell-command-history    . 50)
                tags-file-name
                register-alist)))


;; disable menubar
(if (string-equal system-type "gnu/linux")
    (menu-bar-mode nil)
  (if (string-equal system-type "darwin")
      (setq ns-auto-hide-menu-bar t)))


;; smooth scrolling settings
(setq scroll-step 1
      scroll-conservatively 10000)


;; increase line height
(setq-default line-spacing 4)


;; config Emacs to use $PATH values
(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize)))


;; Emacs autosave, backup, interlocking files
(setq auto-save-default nil
      make-backup-files nil
      create-lockfiles nil)


;; disable startup screen
(setq inhibit-startup-screen t)


;; disable toolbar
(tool-bar-mode -1)


;; enable line numbers
(global-linum-mode t)


;; change linum color
;; TODO - ensure this is evaluated *after* linum is loaded (eval-after-load ...)
(set-face-foreground 'linum "#ddb275")


;; maximize current window
(global-set-key (kbd "M-z") #'delete-other-windows)


;; dired configuration
(require 'dired)
(define-key dired-mode-map (kbd "c") #'find-file) ; this acts as a create-file
(define-key dired-mode-map (kbd "f") #'wpc/find-file)
(define-key dired-mode-map (kbd "-") #'dired-up-directory)

;; dired jumping
(global-set-key (kbd "C-x d") #'dired-jump)

(defun wpc/find-file ()
  "Prefer project-based file-finding if inside of project; otherwise gracefully fallback."
  (interactive)
  (with-current-buffer (current-buffer)
    (if (projectile-project-p)
        (call-interactively #'counsel-projectile-find-file)
      (call-interactively #'find-file))))


;; set default buffer for Emacs
(setq initial-buffer-choice "~/urbint")


;; vim
(use-package evil
  :init
  (defun wpc/evil-window-vsplit-right ()
    (interactive)
    (evil-window-vsplit)
    (windmove-right))
  (defun wpc/evil-window-split-down ()
    (interactive)
    (evil-window-split)
    (windmove-down))
  :config
  (define-key evil-motion-state-map (kbd "RET") #'evil-goto-line)
  (define-key evil-normal-state-map (kbd "H") #'evil-first-non-blank)
  (define-key evil-normal-state-map (kbd "L") #'evil-end-of-line)
  (define-key evil-normal-state-map (kbd "-") #'dired-jump)
  (define-key evil-normal-state-map (kbd "s") nil)
  (define-key evil-normal-state-map (kbd "s l") #'wpc/evil-window-vsplit-right)
  (define-key evil-normal-state-map (kbd "s h") #'evil-window-vsplit)
  (define-key evil-normal-state-map (kbd "s k") #'evil-window-split)
  (define-key evil-normal-state-map (kbd "s j") #'wpc/evil-window-split-down)
  (define-key evil-normal-state-map (kbd "s j") #'wpc/evil-window-split-down)

  ;; paredit specific keybindings
  (require 'paredit)
  (evil-define-key 'normal paredit-mode-map (kbd "==") #'paredit-reindent-defun)
  (evil-define-key 'normal paredit-mode-map (kbd ">)") #'paredit-forward-slurp-sexp)
  (evil-define-key 'normal paredit-mode-map (kbd "<(") #'paredit-backward-slurp-sexp)
  (evil-define-key 'normal paredit-mode-map (kbd "<)") #'paredit-forward-barf-sexp)
  (evil-define-key 'normal paredit-mode-map (kbd ">(") #'paredit-backward-barf-sexp)

  ;; swap two Elements (e is for elements)
  (evil-define-key 'normal paredit-mode-map (kbd ">e") #'paredit-move-forward)
  (evil-define-key 'normal paredit-mode-map (kbd "<e") #'paredit-move-backward)

  ;; swap two Forms (f is for forms)
  (evil-define-key 'normal paredit-mode-map (kbd ">f") #'paredit-move-backward)
  (evil-define-key 'normal paredit-mode-map (kbd "<f") #'paredit-move-backward)

  ;; raising
  (evil-define-key 'normal paredit-mode-map (kbd "go") #'paredit-raise-sexp)

  ;; surround current sexp in a block
  ;; (evil-define-key 'normal paredit-mode-map (kbd "c s e b") nil)

  (require 'cider)
  (evil-define-key 'normal cider-mode-map (kbd "M-.") #'cider-find-var)

  (defun wpc/reindent-defun-and-align-clojure-map ()
    (interactive)
    (call-interactively #'paredit-reindent-defun)
    (call-interactively #'clojure-align))

  (evil-define-key 'normal cider-mode-map (kbd "==") #'wpc/reindent-defun-and-align-clojure-map)

  (define-key evil-ex-completion-map (kbd "M-p") #'previous-complete-history-element)
  (define-key evil-ex-completion-map (kbd "M-n") #'next-complete-history-element)

  ;; unbind ReadLine movements from Evil's insertion map
  (define-key evil-insert-state-map (kbd "C-a") nil)
  (define-key evil-insert-state-map (kbd "C-e") nil)
  (define-key evil-insert-state-map (kbd "C-n") nil)
  (define-key evil-insert-state-map (kbd "C-p") nil)
  (define-key evil-insert-state-map (kbd "C-k") nil)

  (evil-set-initial-state 'help-mode 'emacs)
  (evil-set-initial-state 'cider-repl-mode 'emacs)
  (evil-set-initial-state 'cider-stacktrace-mode 'emacs)
  (evil-set-initial-state 'cider-docview-mode 'emacs)
  (evil-set-initial-state 'cider-browse-spec-mode 'emacs)
  (evil-set-initial-state 'cider-browse-spec-view-mode 'emacs)
  (evil-set-initial-state 'cider-browse-spec-example-mode 'emacs)
  (evil-mode 1)

  ;; create comments easily
  (use-package evil-commentary
    :config
    (evil-commentary-mode))

  ;; evil surround
  (use-package evil-surround
    :config
    (global-evil-surround-mode 1))

  ;; evil leader
  (use-package evil-leader
    :config
    (global-evil-leader-mode)
    (evil-leader/set-leader "<SPC>")

    (defun wpc/evil-leader/set-key-for-clojure-modes (kbd callback)
      (evil-leader/set-key-for-mode 'clojure-mode kbd callback)
      (evil-leader/set-key-for-mode 'clojurec-mode kbd callback)
      (evil-leader/set-key-for-mode 'clojurescript-mode kbd callback))

    (wpc/evil-leader/set-key-for-clojure-modes "d" #'cider-doc)
    (wpc/evil-leader/set-key-for-clojure-modes "e" #'cider-eval-defun-at-point)
    (wpc/evil-leader/set-key-for-clojure-modes "r" #'wpc/find-or-create-clojure-or-clojurescript-repl)
    (wpc/evil-leader/set-key-for-clojure-modes "n" #'wpc/cider/repl-set-ns-and-eval-buffer)

    (evil-leader/set-key
      "i" #'helm-semantic-or-imenu
      "j" #'jump-to-register
      "h" #'help
      "p" #'counsel-git-grep
      "f" #'wpc/find-file
      "b" #'ivy-switch-buffer
      "B" #'ibuffer
      "w" #'ace-select-window)))

(use-package company
  :bind (:map company-active-map
              ("C-j" . company-select-next)
              ("C-n" . company-select-next)
              ("C-k" . company-select-previous)
              ("C-p" . company-select-previous)
              ("C-d" . company-show-doc-buffer))
  :config
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 2)
  (add-hook 'after-init-hook #'global-company-mode))




;; map C-a <-> M-m bindings since M-m is more common
(global-set-key (kbd "C-a") #'back-to-indentation)
(global-set-key (kbd "M-m") #'move-beginning-of-line)


;; fullscreen emacs
(global-set-key (kbd "<s-return>") #'toggle-frame-fullscreen)


;; auto-close parens, brackets, quotes
(electric-pair-mode 1)


;; highlight matching parens, brackets, etc
(show-paren-mode 1)


;; project mgt
(use-package projectile
  :config
  (projectile-mode t))


;; paredit LISP editing
(use-package paredit
  :config
  (define-key paredit-mode-map (kbd "M-q")  nil)
  (add-hook 'clojure-mode-hook #'enable-paredit-mode)
  (add-hook 'clojurescript-mode-hook #'enable-paredit-mode)
  (add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode))


(use-package counsel
  :bind
  ("C-c g" . counsel-git)
  ("C-c j" . counsel-git-grep)
  ("C-h f" . counsel-describe-function)
  ("C-h v" . counsel-describe-function))



;; projectile intergration with ivy
(use-package counsel-projectile
  :config
  (global-set-key (kbd "C-x C-f") #'wpc/find-file)
  (counsel-projectile-on))


;; efficient window-switching
(use-package ace-window
  :bind ("C-x o" . ace-window)
  :config
  (setq aw-keys '(?a ?s ?d ?f ?j ?k ?k ?\;)))


;; create your own vim-like modes
(use-package hydra
  :config
  (defhydra hydra-ibuffer-main (:color pink :hint nil)
    "
 ^Navigation^ | ^Mark^        | ^Actions^        | ^View^
-^----------^-+-^----^--------+-^-------^--------+-^----^-------
  _k_:    ʌ   | _m_: mark     | _D_: delete      | _g_: refresh
 _RET_: visit | _u_: unmark   | _S_: save        | _s_: sort
  _j_:    v   | _*_: specific | _a_: all actions | _/_: filter
-^----------^-+-^----^--------+-^-------^--------+-^----^-------
"
    ("j" ibuffer-forward-line)
    ("RET" ibuffer-visit-buffer :color blue)
    ("k" ibuffer-backward-line)
    ("m" ibuffer-mark-forward)
    ("u" ibuffer-unmark-forward)
    ("*" hydra-ibuffer-mark/body :color blue)
    ("D" ibuffer-do-delete)
    ("S" ibuffer-do-save)
    ("a" hydra-ibuffer-action/body :color blue)
    ("g" ibuffer-update)
    ("s" hydra-ibuffer-sort/body :color blue)
    ("/" hydra-ibuffer-filter/body :color blue)
    ("o" ibuffer-visit-buffer-other-window "other window" :color blue)
    ("q" quit-window "quit ibuffer" :color blue)
    ("." nil "toggle hydra" :color blue))

  (defhydra hydra-ibuffer-mark (:color teal :columns 5
                                       :after-exit (hydra-ibuffer-main/body))
    "Mark"
    ("*" ibuffer-unmark-all "unmark all")
    ("M" ibuffer-mark-by-mode "mode")
    ("m" ibuffer-mark-modified-buffers "modified")
    ("u" ibuffer-mark-unsaved-buffers "unsaved")
    ("s" ibuffer-mark-special-buffers "special")
    ("r" ibuffer-mark-read-only-buffers "read-only")
    ("/" ibuffer-mark-dired-buffers "dired")
    ("e" ibuffer-mark-dissociated-buffers "dissociated")
    ("h" ibuffer-mark-help-buffers "help")
    ("z" ibuffer-mark-compressed-file-buffers "compressed")
    ("b" hydra-ibuffer-main/body "back" :color blue))

  (defhydra hydra-ibuffer-action (:color teal :columns 4
                                         :after-exit
                                         (if (eq major-mode 'ibuffer-mode)
                                             (hydra-ibuffer-main/body)))
    "Action"
    ("A" ibuffer-do-view "view")
    ("E" ibuffer-do-eval "eval")
    ("F" ibuffer-do-shell-command-file "shell-command-file")
    ("I" ibuffer-do-query-replace-regexp "query-replace-regexp")
    ("H" ibuffer-do-view-other-frame "view-other-frame")
    ("N" ibuffer-do-shell-command-pipe-replace "shell-cmd-pipe-replace")
    ("M" ibuffer-do-toggle-modified "toggle-modified")
    ("O" ibuffer-do-occur "occur")
    ("P" ibuffer-do-print "print")
    ("Q" ibuffer-do-query-replace "query-replace")
    ("R" ibuffer-do-rename-uniquely "rename-uniquely")
    ("T" ibuffer-do-toggle-read-only "toggle-read-only")
    ("U" ibuffer-do-replace-regexp "replace-regexp")
    ("V" ibuffer-do-revert "revert")
    ("W" ibuffer-do-view-and-eval "view-and-eval")
    ("X" ibuffer-do-shell-command-pipe "shell-command-pipe")
    ("b" nil "back"))

  (defhydra hydra-ibuffer-sort (:color amaranth :columns 3)
    "Sort"
    ("i" ibuffer-invert-sorting "invert")
    ("a" ibuffer-do-sort-by-alphabetic "alphabetic")
    ("v" ibuffer-do-sort-by-recency "recently used")
    ("s" ibuffer-do-sort-by-size "size")
    ("f" ibuffer-do-sort-by-filename/process "filename")
    ("m" ibuffer-do-sort-by-major-mode "mode")
    ("b" hydra-ibuffer-main/body "back" :color blue))

  (defhydra hydra-ibuffer-filter (:color amaranth :columns 4)
    "Filter"
    ("m" ibuffer-filter-by-used-mode "mode")
    ("M" ibuffer-filter-by-derived-mode "derived mode")
    ("n" ibuffer-filter-by-name "name")
    ("c" ibuffer-filter-by-content "content")
    ("e" ibuffer-filter-by-predicate "predicate")
    ("f" ibuffer-filter-by-filename "filename")
    (">" ibuffer-filter-by-size-gt "size")
    ("<" ibuffer-filter-by-size-lt "size")
    ("/" ibuffer-filter-disable "disable")
    ("b" hydra-ibuffer-main/body "back" :color blue))

  (add-hook 'ibuffer-hook #'hydra-ibuffer-main/body))


;; sensible window mgt
(global-set-key (kbd "M--") #'split-window-below)
(global-set-key (kbd "M-\\") #'split-window-right)
(global-set-key (kbd "M-q") #'delete-window)
(global-set-key (kbd "M-h") #'windmove-left)
(global-set-key (kbd "M-l") #'windmove-right)
(global-set-key (kbd "M-j") #'windmove-down)
(global-set-key (kbd "M-k") #'windmove-up)


;; rebalance emacs windows after splits are created
(defadvice split-window-below (after rebalance-windows activate)
  (balance-windows))

(defadvice split-window-right (after rebalance-windows activate)
  (balance-windows))

(defadvice delete-window (after rebalance-window activate)
  (balance-windows))


;; highlight lines that are over 100 characters long
(use-package whitespace
  :init
  (setq whitespace-line-column 100)
  (setq whitespace-style '(face lines-tail)))


;; trim whitespace on save
(add-hook 'before-save-hook #'delete-trailing-whitespace)


;; disable GUI scrollbars
(when (display-graphic-p)
  (scroll-bar-mode -1))


;; use tabs instead of spaces
(setq-default indent-tabs-mode nil)


;; automatically follow symlinks
(setq vc-follow-symlinks t)


;; fullscreen settings
(setq ns-use-native-fullscreen nil)


;; clojure REPL support
(defun wpc/find-or-create-clojure-or-clojurescript-repl ()
  (interactive)
  (with-current-buffer (current-buffer)
    (let ((buffer-name   (wpc/buffer-name-for-clojure-mode major-mode))
          (repl-function (wpc/repl-function-for-clojure-mode major-mode)))
      (if (get-buffer buffer-name)
          (switch-to-buffer buffer-name)
        (funcall repl-function)))))

(defun wpc/buffer-name-for-clojure-mode (mode)
  (require 'projectile)
  (let* ((project-name (projectile-project-name))
         (cljs-name (concat "*cider-repl CLJS " project-name "*"))
         (clj-name  (concat "*cider-repl " project-name "*")))
    (cond ((eq mode 'clojurescript-mode) cljs-name)
          ((eq mode 'clojure-mode) clj-name)
          ((eq mode 'clojurec-mode) cljs-name))))

(defun wpc/repl-function-for-clojure-mode (mode)
  (require 'projectile)
  (let ((project-name (projectile-project-name))
        (cljs-fn #'cider-jack-in-clojurescript)
        (clj-fn  #'cider-jack-in))
    (cond ((eq mode 'clojurescript-mode) cljs-fn)
          ((eq mode 'clojure-mode) clj-fn)
          ((eq mode 'clojurec-mode) cljs-fn))))

(defun wpc/cider/repl-set-ns-and-eval-buffer ()
  (interactive)
  (with-current-buffer (current-buffer)
    (call-interactively #'cider-eval-buffer)
    (call-interactively #'cider-repl-set-ns)))

(use-package cider
  ;; TODO: define REPL settings as a minor-mode that gets run when specific REPLs boot
  :bind (:map cider-repl-mode-map
              ("C-l" . cider-repl-clear-buffer)
              ("C-u" . kill-whole-line))
  :init
  (setq cider-cljs-lein-repl
        "(do (require 'figwheel-sidecar.repl-api)
             (figwheel-sidecar.repl-api/start-figwheel!)
             (figwheel-sidecar.repl-api/cljs-repl))")
  (setq cider-prompt-for-symbol nil)
  :config
  (define-key cider-repl-mode-map (kbd "<up>") #'cider-repl-previous-input)
  (define-key cider-repl-mode-map (kbd "<down>") #'cider-repl-next-input)
  (define-key clojurescript-mode-map (kbd "C-c M-j") #'wpc/find-or-create-clojure-or-clojurescript-repl))



;; premium Emacs themes
(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t)
  doom-themes-enable-italic t
  (load-theme 'doom-one t)
  (doom-themes-visual-bell-config)
  (doom-themes-org-config))


;; kbd discovery
(use-package which-key
  :init
  (setq which-key-idle-delay 0.25)
  :config
  (which-key-mode))


;; targeted line jumping Emacs style
(use-package avy
  :bind
  (("C-'" . avy-goto-char)))


;; completion framework
(use-package ivy
  :config
  (use-package all-the-icons-ivy
    :config
    (all-the-icons-ivy-setup))
  (ivy-mode t))


;; client for git
(use-package magit)


;; ido-based M-x
(use-package smex
  :bind
  ("M-x" . smex)
  ("M-X" . smex-major-mode-commands)
  :init
  ;; smex (ido) configuration
  (require 'ido)
  (defun wpc/bind-ido-keys ()
    "Adds custom KBDs for ido. This function is recommended in the ido source code."
    (define-key ido-completion-map (kbd "<tab>") #'ido-next-match)
    (define-key ido-completion-map (kbd "<backtab>") #'ido-prev-match))
  (add-hook 'ido-setup-hook #'wpc/bind-ido-keys)
  :config
  (smex-initialize))


;; interactive buffer searching
(use-package swiper
  :bind
  ("C-s" . swiper)
  ("C-r" . swiper))


(use-package markdown-mode)
