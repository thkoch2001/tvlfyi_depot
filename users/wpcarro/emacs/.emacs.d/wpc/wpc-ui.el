;;; wpc-ui.el --- Any related to the UI/UX goes here -*- lexical-binding: t -*-

;; Author: William Carroll <wpcarro@gmail.com>
;; Version: 0.0.1
;; Package-Requires: ((emacs "24"))

;;; Commentary:
;; Hosts font settings, scrolling, color schemes.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dependencies
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'constants)
(require 'prelude)
(require 'al)
(require 'fonts)
(require 'theme)
(require 'device)
(require 'laptop-battery)
(require 'modeline)
(require 'general)
(require '>)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; line height
(setq-default line-spacing 0)

(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b"))))

;; Ensure that buffers update when their contents change on disk.
(global-auto-revert-mode t)

;; smooth scrolling settings
(setq scroll-step 1
      scroll-conservatively 10000)

;; clean up modeline
(use-package diminish
  :config
  (diminish 'emacs-lisp-mode "elisp")
  (diminish 'evil-commentary-mode)
  (diminish 'flycheck-mode)
  (diminish 'auto-revert-mode)
  (diminish 'which-key-mode)
  (diminish 'yas-minor-mode)
  (diminish 'lispyville-mode)
  (diminish 'undo-tree-mode)
  (diminish 'company-mode)
  (diminish 'projectile-mode)
  (diminish 'eldoc-mode)
  ;; This is how to diminish `auto-fill-mode'.
  (diminish 'auto-fill-function)
  (diminish 'counsel-mode)
  (diminish 'ivy-mode))

;; TODO: Further customize `mode-line-format' variable.
(delete 'mode-line-modes mode-line-format)
(delete '(vc-mode vc-mode) mode-line-format)

;; disable startup screen
(setq inhibit-startup-screen t)

;; disable toolbar
(tool-bar-mode -1)

;; premium Emacs themes
(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (doom-themes-visual-bell-config)
  (doom-themes-org-config))

;; kbd discovery
(use-package which-key
  :config
  (setq which-key-idle-delay 0.25)
  (which-key-mode))

;; completion framework
(use-package ivy
  :config
  (counsel-mode t)
  (ivy-mode t)
  ;; Remove preceding "^" from ivy prompts
  (setq ivy-initial-inputs-alist nil)
  ;; prefer using `helpful' variants
  (progn
    (setq counsel-describe-function-function #'helpful-callable)
    (setq counsel-describe-variable-function #'helpful-variable))
  (general-define-key
   :keymaps '(ivy-minibuffer-map ivy-switch-buffer-map)
   ;; prev
   "C-k" #'ivy-previous-line
   "<backtab>" #'ivy-previous-line
   ;; next
   "C-j" #'ivy-next-line
   "<tab>" #'ivy-next-line))

(use-package ivy-prescient
  :config
  (ivy-prescient-mode 1)
  (unless constants-ci?
    (prescient-persist-mode 1)))

(use-package ivy-pass)

;; all-the-icons
(use-package all-the-icons
  :config
  (unless (or constants-ci?
              (f-exists? "~/.local/share/fonts/all-the-icons.ttf"))
    (all-the-icons-install-fonts t)))

;; icons for Ivy
(use-package all-the-icons-ivy
  :after (ivy all-the-icons)
  :config
  (all-the-icons-ivy-setup))

;; disable menubar
(menu-bar-mode -1)

;; reduce noisiness of auto-revert-mode
(setq auto-revert-verbose nil)

;; highlight lines that are over 80 characters long
(use-package whitespace
  :config
  ;; TODO: This should change depending on the language and project. For
  ;; example, Google Java projects prefer 100 character width instead of 80
  ;; character width.
  (setq whitespace-line-column 80)
  (setq whitespace-style '(face lines-tail))
  (add-hook 'prog-mode-hook #'whitespace-mode))

;; dirname/filename instead of filename<dirname>
(setq uniquify-buffer-name-style 'forward)

;; highlight matching parens, brackets, etc
(show-paren-mode 1)

;; hide the scroll-bars in the GUI
(scroll-bar-mode -1)

;; TODO: Learn how to properly integrate this with dunst or another system-level
;; notification program.
;; GUI alerts in emacs
(use-package alert
  :commands (alert)
  :config
  (setq alert-default-style 'notifier))

;; TODO: Should `device-laptop?' be a function or a constant that gets set
;; during initialization?
(when (device-laptop?) (laptop-battery-display))

(setq theme-whitelist
      (->> (custom-available-themes)
           (list-map #'symbol-name)
           (list-filter (>-> (s-starts-with? "doom-")))
           (list-map #'intern)))
(setq theme-linum-color-override "da5478")
(add-hook 'theme-after-change
          (lambda () (prelude-set-line-number-color "#da5478")))
(theme-whitelist-set 'doom-flatwhite)

(when window-system
  ;; On OSX, JetBrainsMono is installed as "JetBrains Mono", and I'm
  ;; not sure how to change that.
  (let ((font (if constants-osx? "JetBrains Mono" "JetBrainsMono")))
    (fonts-set font)
    ;; Some themes (e.g. doom-acario-*) change the font for comments. This
    ;; should prevent that.
    (set-face-attribute font-lock-comment-face nil
                        :family font
                        :slant 'normal)))

(modeline-setup)

(provide 'wpc-ui)
;;; wpc-ui.el ends here
