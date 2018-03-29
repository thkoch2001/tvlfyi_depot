;;; private/grfn/config.el -*- lexical-binding: t; -*-

(defvar +grfn-dir (file-name-directory load-file-name))
(defvar +grfn-snippets-dir (expand-file-name "snippets/" +grfn-dir))

;;
(when (featurep! :feature evil)
  (load! +bindings)
  (load! +commands))

(load! +private)

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

;; completion/helm
(after! helm
  ;; Hide header lines in helm. I don't like them
  (set-face-attribute 'helm-source-header nil :height 0.1))

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

(setq solarized-use-variable-pitch nil
      solarized-scale-org-headlines nil)

; (require 'doom-themes)

;; Should really figure out which of these is correct, eventually

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
   '(("t" "Todo" entry
      (file+headline +org-default-todo-file "Inbox")
      "* TODO %?\n%i" :prepend t :kill-buffer t)

     ("n" "Notes" entry
      (file+headline +org-default-notes-file "Inbox")
      "* %u %?\n%i" :prepend t :kill-buffer t))
   org-deadline-warning-days 1
   org-agenda-skip-scheduled-if-deadline-is-shown 't)
  (set-face-foreground 'org-block +solarized-s-base00)
  (add-hook! org-mode
    (add-hook! evil-normal-state-entry-hook
      #'org-align-all-tags))
  (setf (alist-get 'file org-link-frame-setup) 'find-file-other-window)
  (set-face-foreground 'org-block +solarized-s-base00))

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
        ("<>"        . "⊕"))))
  (dolist (item m-symbols) (add-to-list 'haskell-font-lock-symbols-alist item)))

(setq haskell-font-lock-symbols t)


(add-hook! haskell-mode
  (intero-mode)
  (flycheck-add-next-checker
   'intero
   'haskell-hlint)
  (set-fill-column 100))

;; (load! org-clubhouse)
(add-hook! org-mode #'org-clubhouse-mode)

(load! slack-snippets)

(after! magit
  (require 'evil-magit)
  (require 'magithub)
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
(add-hook 'prog-mode-hook #'fci-mode)
(after! fill-column-indicator
  (add-hook 'prog-mode-hook #'fci-mode)
  (defvar eos/fci-disabled nil)
  (make-variable-buffer-local 'eos/fci-disabled)

  ;; Add a hook that disables fci if enabled when the window changes and it
  ;; isn't wide enough to display it.
  (defun eos/maybe-disable-fci ()
    (interactive)
    ;; Disable FCI if necessary
    (when (and fci-mode
               (< (window-width) (or fci-rule-column fill-column)))
      (fci-mode -1)
      (setq-local eos/fci-disabled t))
    ;; Enable FCI if necessary
    (when (and eos/fci-disabled
               (eq fci-mode nil)
               (> (window-width) (or fci-rule-column fill-column)))
      (fci-mode 1)
      (setq-local eos/fci-disabled nil)))

  (defun eos/add-fci-disabling-hook ()
    (interactive)
    (add-hook 'window-configuration-change-hook
              #'eos/maybe-disable-fci))

  (add-hook 'prog-mode-hook #'eos/add-fci-disabling-hook))


;;; Javascript

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
  (magit-commit "-m wip"))

;; (magit-define-popup-action 'magit-commit-popup
;;   ?w "WIP" 'magit-commit-wip)

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
;;   (lsp-mode))

;; (def-package! lsp-ui
;;   :after lsp-mode
;;   :config
;;   (setq lsp-ui-flycheck-enable t)
;;   (setq imenu-auto-rescan t)
;;   (set-face-background 'lsp-ui-doc-background +solarized-s-base2)
;;   (set-face-background 'lsp-face-highlight-read +solarized-s-base2)
;;   (set-face-background 'lsp-face-highlight-orite +solarized-s-base2)
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
;;   (haskell-mode . lsp-haskell-enable))

(def-package! evil-magit
  :after (magit))

(def-package! writeroom-mode)
