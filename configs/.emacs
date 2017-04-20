;;; William Carroll's Emacs configuration


;; From `https://github.com/melpa/melpa`
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/")))
(package-initialize)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   (vector "#eaeaea" "#d54e53" "#b9ca4a" "#e7c547" "#7aa6da" "#c397d8" "#70c0b1" "#424242"))
 '(column-number-mode t)
 '(custom-enabled-themes (quote (sanityinc-tomorrow-eighties)))
 '(custom-safe-themes
   (quote
    ("628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "398e6465d45d5af4cbf94f8ebfb24deb71249f28cdfb4b0fa7197354ee0c9802" "db34c17b9a7810856352a341e15d701696fb4710fe7e0dab57b8268515c2b082" "ee93cac221c92b580bde1326209e1a327287cd49931ba319a9af7a7af201967c" "68f66d916f4e90f11f2dc815e9580c1aaf9e9c75eeee3fbd8b663d706e121a1a" "c158c2a9f1c5fcf27598d313eec9f9dceadf131ccd10abc6448004b14984767c" "cc0dbb53a10215b696d391a90de635ba1699072745bf653b53774706999208e3" "3e335d794ed3030fefd0dbd7ff2d3555e29481fe4bbb0106ea11c660d6001767" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "20e23cba00cf376ea6f20049022241c02a315547fc86df007544852c94ab44cb" "60d4556ebff0dc94849f177b85dcb6956fe9bd394c18a37e339c0fcd7c83e4a9" "707227acad0cf8d4db55dcf1e574b3644b68eab8aca4a8ce6635c8830bc72144" "1c656eb3f6ae6c84ced46282cb4ed697bffe2f6c764bb5a737ed7ca6d068f798" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "9f3181dc1fabe5d58bbbda8c48ef7ece59b01bed606cfb868dd147e8b36af97c" "ad1c2abad40e11d22156fe3987fd9b74b9e1c822264a07dacb24e0b3133aaed1" "945fe66fbc30a7cbe0ed3e970195a7ee79ee34f49a86bc96d02662ab449b8134" "0f0db69b7a75a7466ef2c093e127a3fe3213ce79b87c95d39ed1eccd6fe69f74" "08b8807d23c290c840bbb14614a83878529359eaba1805618b3be7d61b0b0a32" "9d91458c4ad7c74cf946bd97ad085c0f6a40c370ac0a1cbeb2e3879f15b40553" "6254372d3ffe543979f21c4a4179cd819b808e5dd0f1787e2a2a647f5759c1d1" "8ec2e01474ad56ee33bc0534bdbe7842eea74dccfb576e09f99ef89a705f5501" "5b24babd20e58465e070a8d7850ec573fe30aca66c8383a62a5e7a3588db830b" "eb0a314ac9f75a2bf6ed53563b5d28b563eeba938f8433f6d1db781a47da1366" "3d47d88c86c30150c9a993cc14c808c769dad2d4e9d0388a24fee1fbf61f0971" default)))
 '(evil-shift-width 2)
 '(fci-rule-color "#424242")
 '(mouse-wheel-mode nil)
 '(neo-window-fixed-size nil)
 '(neo-window-width 35)
 '(package-selected-packages
   (quote
    (yaml-mode haskell-mode color-theme-sanityinc-tomorrow graphql-mode flycheck-elm popup-kill-ring flycheck-pos-tip green-phosphor-theme green-screen-theme minimal-theme creamsody-theme autothemer solarized-theme avk-emacs-themes github-theme all-the-icons-dired ace-window yasnippet chess synonyms powerline doom-neotree doom-themes persp-mode use-package helm-projectile persp-projectile perspective projectile with-editor helm-core company helm-ag evil-leader flycheck-mix flycheck-elixir evil-matchit typescript-mode evil-surround erlang elixir-mode golden-ratio flycheck-credo flycheck command-log-mode atom-one-dark-theme exec-path-from-shell clues-theme gotham-theme dracula-theme zenburn-theme fill-column-indicator neotree evil iedit vimrc-mode helm-ispell transpose-frame helm-ack nyan-mode alchemist helm magit dockerfile-mode elm-mode ack)))
 '(popwin-mode t)
 '(popwin:popup-window-height 25)
 '(popwin:special-display-config
   (quote
    (help-mode
     ("^*helm-.+*$" :regexp t)
     ("^*helm .+*$" :regexp t)
     ("^*helm-.+*$" :regexp t)
     ("^*helm .+*$" :regexp t)
     ("^*helm .+*$" :regexp t)
     ("*Miniedit Help*" :noselect t)
     (completion-list-mode :noselect t)
     (compilation-mode :noselect t)
     (grep-mode :noselect t)
     (occur-mode :noselect t)
     ("*Pp Macroexpand Output*" :noselect t)
     "*Shell Command Output*" "*vc-diff*" "*vc-change-log*"
     (" *undo-tree*" :width 60 :position right)
     ("^\\*anything.*\\*$" :regexp t)
     "*slime-apropos*" "*slime-macroexpansion*" "*slime-description*"
     ("*slime-compilation*" :noselect t)
     "*slime-xref*"
     (sldb-mode :stick t)
     slime-repl-mode slime-connection-list-mode)))
 '(pos-tip-background-color "red")
 '(tool-bar-mode nil)
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#d54e53")
     (40 . "#e78c45")
     (60 . "#e7c547")
     (80 . "#b9ca4a")
     (100 . "#70c0b1")
     (120 . "#7aa6da")
     (140 . "#c397d8")
     (160 . "#d54e53")
     (180 . "#e78c45")
     (200 . "#e7c547")
     (220 . "#b9ca4a")
     (240 . "#70c0b1")
     (260 . "#7aa6da")
     (280 . "#c397d8")
     (300 . "#d54e53")
     (320 . "#e78c45")
     (340 . "#e7c547")
     (360 . "#b9ca4a"))))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(hl-line ((t (:inherit nil)))))


;; Turn off line-wrapping (default)
(set-default 'truncate-lines t)
(setq truncate-partial-width-windows nil)

(setq css-indent-offset 2)
(setq js-indent-level 2)

;; Window Auto-Balancing
(defadvice split-window-below (after restore-balanace-below activate)
  (balance-windows))

(defadvice split-window-right (after restore-balance-right activate)
  (balance-windows))

(defadvice delete-window (after restore-balance activate)
  (balance-windows))


;; Smart mode line
(use-package smart-mode-line
  :ensure t
  :init
  (load-theme 'smart-mode-line-dark t)
  :config
  (setq sml/no-confirm-load-theme t)
  (sml/setup))


;; Disable fringes in Emacs
(fringe-mode 0)


(setq linum-format " %d  ")


;; Command Log Mode
(use-package command-log-mode
  :ensure t
  :config
  (setq-default command-log-mode-window-font-size 1)
  (setq-default command-log-mode-window-size 40))


;; Ace Window
(use-package ace-window
  :ensure t
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))


;; All-the-fonts
(use-package all-the-icons
  :ensure t)


;; Thesaurus
(use-package synonyms
  :ensure t)


;; Doom Themes
(use-package doom-themes
  :ensure t
  :init
  (use-package doom-nlinum))

;; Magit Settings
(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch-popup)))


;; View stream of Emacs commands
(use-package command-log-mode
  :ensure t
  :commands (global-command-log-mode))


;; Flycheck Settings
(use-package flycheck
  :ensure t)


;; Ansi Term
(use-package term
  :bind (:map term-mode-map
         ("M-p" . term-send-up)
         ("M-n" . term-send-down)

         :map term-raw-map
         ("C-h" . evil-window-left)
         ("C-l" . evil-window-right)
         ("C-k" . evil-window-up)
         ("C-j" . evil-window-down)))


;; Projectile Settings
(use-package projectile
  :ensure t
  :commands (projectile-mode))


;; Dired Settings
(use-package dired
  :bind (:map dired-mode-map
        ("c" . find-file)
        ("K" . dired-up-directory)))


;; Evil Settings
(use-package evil
  :ensure t
  :commands (evil-mode local-evil-mode)
  :bind (:map evil-visual-state-map
         ("H" . evil-first-non-blank)
         ("L" . evil-end-of-visual-line)

         :map evil-motion-state-map
         ("<return>" . nil)
         ("<tab>" . nil)
         ("SPC" . nil)
         ("M-." . nil)

         :map evil-insert-state-map
         ("C-k" . nil)

         :map evil-normal-state-map
         ("<return>" . nil)
         ("<tab>" . nil)
         ("K" . nil)
         ("M-." . nil)
         ("s" . nil)
         ("C-p" . nil)
         ("C-h" . evil-window-left)
         ("C-l" . evil-window-right)
         ("C-k" . evil-window-up)
         ("C-j" . evil-window-down)
         ("g c" . comment-or-uncomment-region)
         ("s h" . evil-window-vsplit)
         ("s l" . evil-window-vsplit-right)
         ("s k" . evil-window-split)
         ("s j" . evil-window-split-down)
         ("H" . evil-first-non-blank)
         ("L" . evil-end-of-line)
         ("<S-left>" . evil-window-increase-width)
         ("<S-right>" . evil-window-decrease-width)
         ("<S-up>" . evil-window-decrease-height)
         ("<S-down>" . evil-window-increase-height)

         :map evil-ex-map
         ("tb" . alchemist-test-this-buffer)
         ("tap" . alchemist-test-at-point)
         ("lt" . alchemist-mix-rerun-last-test)
         )
  :init
  (setq evil-emacs-state-cursor '("VioletRed3" box))
  (setq evil-normal-state-cursor '("DeepSkyBlue2" box))
  (setq evil-visual-state-cursor '("orange" box))
  (setq evil-insert-state-cursor '("VioletRed3" bar))
  (setq evil-replace-state-cursor '("VioletRed3" bar))
  (setq evil-operator-state-cursor '("VioletRed3" hollow))
  (global-evil-matchit-mode t)
  (global-evil-surround-mode t)
  (global-evil-leader-mode t))


  (add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")


  (defun evil-window-vsplit-right ()
    "Vertically split a window and move right."
    (interactive)
    (evil-window-vsplit nil)
    (evil-window-right 1))

  (defun evil-window-split-down ()
    "Split a window and move right."
    (interactive)
    (evil-window-split nil)
    (evil-window-down 1))


  ;; Evil Leader Settings
  (use-package evil-leader
    :ensure t
    :commands (global-evil-leader-mode)
    :config
    (evil-leader/set-leader "<SPC>")
    (evil-leader/set-key
      "w" 'toggle-truncate-lines
      "x" 'helm-M-x
      "<SPC>" 'mode-line-other-buffer
      "a" 'ace-window
      "n" 'neotree-toggle-project-dir
      "N" 'neotree-reveal-current-buffer
      "t" 'alchemist-project-toggle-file-and-tests
      "f" 'helm-projectile
      "p" 'helm-projectile-ag
      "d" 'dired-jump
      "D" 'projectile-dired
      "q" 'kill-this-buffer
      "h" 'evil-window-left
      "i" 'helm-semantic-or-imenu
      "l" 'evil-window-right
      "k" 'evil-window-up
      "s" 'load-unfriendly-theme
      "S" 'load-friendly-theme
      "j" 'evil-window-down
      "b" 'helm-mini
      "T" 'alchemist-mix-test-at-point
      "B" 'alchemist-mix-test-this-buffer
      "L" 'alchemist-mix-rerun-last-test
      "g" 'magit-status
      ))


  ;; Evil Match-it
  (use-package evil-matchit
    :ensure t
    :commands (global-evil-matchit-mode))


  ;; Evil Surround
  (use-package evil-surround
    :ensure t
    :commands (global-evil-surround-mode))


  ;; Flycheck Mix Settings
  (use-package flycheck-mix
    :ensure t
    :init
    (flycheck-mix-setup))


  ;; Flycheck
  (use-package flycheck
    :ensure t
    :config
    (setq flycheck-display-errors-function 'ignore)
    (flycheck-pos-tip-mode))


  ;; Flycheck Credo Settings
  (use-package flycheck-credo
    :ensure t
    :init
    (flycheck-credo-setup))


  ;; Popwin Settings
  (use-package popwin
    :ensure t)

  (defun *-popwin-help-mode-off ()
    "Turn `popwin-mode' off for *Help* buffers."

    (when (boundp 'popwin:special-display-config)
      (customize-set-variable 'popwin:special-display-config
                              (delq 'help-mode popwin:special-display-config))))

  (defun *-popwin-help-mode-on ()
    "Turn `popwin-mode' on for *Help* buffers."

    (when (boundp 'popwin:special-display-config)
      (customize-set-variable 'popwin:special-display-config
                              (add-to-list 'popwin:special-display-config 'help-mode nil #'eq))))

  (add-hook 'helm-minibuffer-set-up-hook #'*-popwin-help-mode-off)
  (add-hook 'helm-cleanup-hook #'*-popwin-help-mode-on)

  (setq display-buffer-function 'popwin:display-buffer)
  (setq helm-split-window-preferred-function 'ignore)
  (push '("^\*helm .+\*$" :regexp t) popwin:special-display-config)
  (push '("^\*helm-.+\*$" :regexp t) popwin:special-display-config)


  ;; Alchemist Settings
  (use-package alchemist
    :ensure t
    :config
    (setq alchemist-mix-env "prod")
    (setq alchemist-goto-elixir-source-dir "~/source_code/elixir/")
    (setq alchemist-goto-erlang-source-dir "~/source_code/otp/")
    :init
    (linum-mode))

  (defun custom-erlang-mode-hook ()
    "Jump to and from Elixir, Erlang, Elixir files."
    (define-key erlang-mode-map (kbd "M-,") 'alchemist-goto-jump-back))

  (add-hook 'erlang-mode-hook 'custom-erlang-mode-hook)


  (defun helm-ag-neotree-node ()
    "Run Helm-ag on Neotree directory."
    (interactive)
    (let ((search-root (neo-buffer--get-filename-current-line)))
      (if search-root
          ;; search directory
          (progn
            (evil-window-right 1)
            (helm-ag search-root))
        (message "Could not find directory at point."))))


  ;; NeoTree Settings
  (use-package neotree
    :ensure t
    :bind (:map neotree-mode-map
          ("j" . next-line)
          ("k" . previous-line)
          ("<return>" . neotree-enter)
          ("<tab>" . neotree-enter)
          ("D" . neotree-delete-node)
          ("R" . neotree-rename-node)
          ("c" . neotree-create-node)
          ("C-h" . evil-window-left)
          ("C-l" . evil-window-right)
          ("C-k" . evil-window-up)
          ("C-j" . evil-window-down)
          ("C-p" . helm-ag-neotree-node)
          )
    :init
    (hl-line-mode)
    (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
    (setq-default neo-show-hidden-files t))

  (defun neotree-toggle-project-dir ()
    "Toggle neotree sidebar."
    (interactive)
    (let ((project-dir (projectile-project-root))
          (file-name (buffer-file-name)))
      (neotree-toggle)
      (if project-dir
          (if (neo-global--window-exists-p)
              (progn
                (neotree-dir project-dir)
                (neotree-show)
                (evil-window-mru)))
        (message "Could not find git project root."))))


  (defun neotree-reveal-current-buffer ()
    "Reveal current buffer in Neotree."
    (interactive)
    (let ((project-dir (projectile-project-root))
          (file-name (buffer-file-name)))
      (neotree-show)
      (if project-dir
          (if (neo-global--window-exists-p)
              (progn
                (neotree-dir project-dir)
                (neotree-find file-name)
                (evil-window-mru)))
        (message "Could not find git project root."))))


  (defun message-project-root ()
    "Outputs project-root."
    (interactive)
    (let (project-dir (projectile-project-root))
      (if project-dir
          (message "Project dir found!")
        (message "No project-dir found."))))


  ;; Whitespace Settings
  (use-package whitespace
    :ensure t
    :commands (whitespace-mode)
    :config
    (setq whitespace-line-column 80)
    (setq whitespace-style '(face lines-tail)))


  ;; Helm Settings
  (use-package helm
    :ensure t
    :commands (helm-mode)
    :bind (
          ("M-x" . helm-M-x)
          ("M-y" . helm-show-kill-ring)
          ("C-x b" . helm-mini)
          :map helm-map
          ("TAB" . helm-execute-persistent-action)
          ("C-z" . helm-select-action)
          :term-raw-map
          ("M-x" . helm-M-x))
    :init
    (setq helm-buffers-fuzzy-matching t)
    (setq helm-recentf-fuzzy-match t)
    (setq helm-semantic-fuzzy-match t)
    (setq helm-imenu-fuzzy-match t)
    (setq helm-locate-fuzzy-match t))


  ;; Helm Projectile Settings
  (use-package helm-projectile
    :ensure t)


  ;; Elm Mode
  (use-package elm-mode
    :config
    (add-to-list 'company-backends 'company-elm))


  ;; Company Settings
  (use-package company
    :bind (
          ("C-j" . company-select-next)
          ("C-k" . company-select-previous))
    :config
    (setq company-idle-delay 0))


  (add-hook 'after-init-hook 'evil-mode)
  (add-hook 'after-init-hook 'global-whitespace-mode)
  (add-hook 'after-init-hook 'global-hl-line-mode)
  (add-hook 'after-init-hook 'global-linum-mode)
  (add-hook 'after-init-hook 'global-flycheck-mode)
  (add-hook 'after-init-hook 'global-company-mode)
  (add-hook 'after-init-hook 'projectile-mode)
  (add-hook 'after-init-hook 'helm-mode)
  (add-hook 'before-save-hook 'delete-trailing-whitespace)


  ;; Scrolling Settings
  (setq scroll-step 1)
  (setq scroll-conservatively 10000)


  ;; Properly configure GUI Emacs to use $PATH values
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize))


  ;; Emacs backup / autosave files
  ;; (setq-default make-backup-files nil)
  (setq backup-directory-alist `(("." . "~/.emacs-tmp")))
  (setq auto-save-file-name-transforms `((".*" "~/.emacs-tmp/" t)))


  ;; Automatically follow symlinks
  (setq vc-follow-symlinks t)


  ;; Commenting / Uncommenting
  (global-set-key (kbd "C-x C-;") 'comment-or-uncomment-region)


  ;; Fullscreen settings
  (setq ns-use-native-fullscreen nil)
  (global-set-key (kbd "<s-return>") 'toggle-frame-fullscreen)


  ;; General Settings
  ;; Hide the menu-bar
  (setq ns-auto-hide-menu-bar t)

  ;; Native App Settings
  (tool-bar-mode -1)

  ;; Disable GUI scrollbars
  (when (display-graphic-p)
    (scroll-bar-mode -1)
    )

  ;; Use spaces instead of tabs
  (setq-default indent-tabs-mode nil)

  ;; Change font settings
  (add-to-list 'default-frame-alist '(font . "Operator Mono"))


  ;; Force save buffers
  (defun save-buffer-always ()
    "Save the buffer even if it is not modified."
    (interactive)
    (set-buffer-modified-p t)
    (save-buffer))

  (global-set-key (kbd "C-x C-s") nil)
  (global-set-key (kbd "C-x C-s") 'save-buffer-always)


  ;; Upgrade all packages
  (defun package-upgrade-all ()
    "Upgrade all packages automatically without showing *Packages* buffer."
    (interactive)
    (package-refresh-contents)
    (let (upgrades)
      (cl-flet ((get-version (name where)
                  (let ((pkg (cadr (assq name where))))
                    (when pkg
                      (package-desc-version pkg)))))
        (dolist (package (mapcar #'car package-alist))
          (let ((in-archive (get-version package package-archive-contents)))
            (when (and in-archive
                      (version-list-< (get-version package package-alist)
                                      in-archive))
              (push (cadr (assq package package-archive-contents))
                    upgrades)))))
      (if upgrades
          (when (yes-or-no-p
                (message "Upgrade %d package%s (%s)? "
                          (length upgrades)
                          (if (= (length upgrades) 1) "" "s")
                          (mapconcat #'package-desc-full-name upgrades ", ")))
            (save-window-excursion
              (dolist (package-desc upgrades)
                (let ((old-package (cadr (assq (package-desc-name package-desc)
                                              package-alist))))
                  (package-install package-desc)
                  (package-delete  old-package)))))
        (message "All packages are up to date"))))


  ;; Colorscheme
(load-theme 'atom-one-dark t)


;; Add transparency
(set-frame-parameter (selected-frame) 'alpha '(90 . 90))
(add-to-list 'default-frame-alist '(alpha . (90 . 90)))
