;;; javascript.el --- My Javascript preferences -*- lexical-binding: t -*-
;; Author: William Carroll <wpcarro@gmail.com>

;;; Commentary:
;; This module hosts my Javascript tooling preferences

;;; Code:

;; Helper functions
(defun wpc/indium-setup (url)
  "Setup the indium environment using URL."
  (indium-eval (format "window.dispatchEvent(new CustomEvent('patch', {detail: {url: %s}}" url)))

(defun wpc/insert-flow-annotation ()
  "Insert a flow type annotation to the beginning of a buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (insert "// @flow\n")))

;; ;; javascript
;; (evil-leader/set-key-for-mode 'rjsx-mode "t" #'wpc/toggle-between-js-test-and-module)
;; (evil-leader/set-key-for-mode 'rjsx-mode "x" #'wpc/toggle-between-js-component-and-store)
;; (evil-leader/set-key-for-mode 'rjsx-mode "u" #'wpc/jump-to-parent-file)

;; javascript setup
(use-package indium
  :hook (indium-update-script-source . wpc/indium-setup))

;; javascript text objects
(quelpa '(evil-text-objects-javascript
          :fetcher github
          :repo "urbint/evil-text-objects-javascript"))
(require 'evil-text-objects-javascript)

;; Flow for Javascript
(use-package flow-minor-mode
  :hook js2-mode
  :requires evil-leader
  :config
  (evil-leader/set-key-for-mode 'rjsx-mode "F" #'wpc/insert-flow-annotation))

(use-package company-flow
  :after (company)
  :hook (rjsx-mode . wpc/set-flow-executable)
  :config
  (add-to-list 'company-flow-modes 'rjsx-mode)
  (add-to-list 'company-backends 'company-flow))

(use-package flycheck-flow
  :after (flycheck)
  :config
  (flycheck-add-mode 'javascript-flow 'rjsx-mode)
  (flycheck-add-mode 'javascript-flow 'flow-minor-mode)
  (flycheck-add-mode 'javascript-eslint 'flow-minor-mode)
  (flycheck-add-next-checker 'javascript-flow 'javascript-eslint))

;; front-end indentation
(setq js-indent-level 2
      css-indent-offset 2)

;; eslint integration with flycheck
(setq flycheck-javascript-eslint-executable "~/urbint/grid-front-end/node_modules/.bin/eslint")

;; JS autoformatting
(use-package prettier-js
  :after (rjsx-mode)
  :ghook ('(rjsx-mode-hook
            js2-mode-hook
            json-mode-hook
            css-mode-hook)))

;; JSX highlighting
(use-package rjsx-mode
  :after (evil-text-objects-javascript)
  :general
  (general-unbind rjsx-mode-map "<" ">" "C-d")
  (n rjsx-mode-map
     "K" 'flow-minor-type-at-pos)
  :gfhook #'evil-text-objects-javascript/install
  :mode "\\.js\\'"
  :config
  (setq js2-mode-show-parse-errors nil
        js2-mode-show-strict-warnings nil))

(provide 'wpc-javascript)
;;; wpc-javascript.el ends here
