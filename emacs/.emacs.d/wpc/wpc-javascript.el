;; wpc-javascript.el --- My Javascript preferences -*- lexical-binding: t -*-
;; Author: William Carroll <wpcarro@gmail.com>

;;; Commentary:
;; This module hosts my Javascript tooling preferences.  This also includes
;; tooling for TypeScript and other frontend tooling.  Perhaps this module will
;; change names to more accurately reflect that.
;;
;; Depends
;; - yarn global add prettier

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Constants
(defconst wpc/js-hooks
  '(js-mode-hook web-mode-hook typescript-mode-hook js2-mode-hook rjsx-mode-hook)
  "All of the commonly used hooks for Javascript buffers.")

(defconst wpc/frontend-hooks
  (-insert-at 0 'css-mode-hook wpc/js-hooks)
  "All of the commonly user hooks for frontend development.")


;; frontend indentation settings
(setq typescript-indent-level 2
      js-indent-level 2
      css-indent-offset 2)

;; Flow for Javascript
(use-package add-node-modules-path
  :config
  (general-add-hook wpc/js-hooks #'add-node-modules-path))

(use-package web-mode
  :mode "\\.html\\'"
  :config
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-markup-indent-offset 2))

;; JSX highlighting
(use-package rjsx-mode
  :config
  (general-unbind rjsx-mode-map "<" ">" "C-d")
  (general-nmap
    :keymaps 'rjsx-mode-map
    "K" #'flow-minor-type-at-pos)
  (setq js2-mode-show-parse-errors nil
        js2-mode-show-strict-warnings nil))

(progn
  (defun tide/setup ()
    (interactive)
    (tide-setup)
    (flycheck-mode 1)
    (setq flycheck-check-syntax-automatically '(save mode-enabled))
    (eldoc-mode 1)
    (tide-hl-identifier-mode 1)
    (company-mode 1))
  (use-package tide
    :config
    (add-hook 'typescript-mode-hook #'tide/setup))
  (require 'web-mode)
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
  (add-hook 'web-mode-hook
            (lambda ()
              (when (string-equal "tsx" (f-ext buffer-file-name))
                (tide/setup))))
  (flycheck-add-mode 'typescript-tslint 'web-mode))

;; JS autoformatting
(use-package prettier-js
  :config
  (general-add-hook wpc/frontend-hooks #'prettier-js-mode))

(provide 'wpc-javascript)
;;; wpc-javascript.el ends here
