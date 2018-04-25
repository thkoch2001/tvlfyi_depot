;;; company.el --- Autocompletion package, company, preferences -*- lexical-binding: t -*-
;; Author: William Carroll <wpcarro@gmail.com>

;;; Commentary:
;; Hosts my company mode preferences

;;; Code:

;; autocompletion client
(use-package company
  :general
  (company-active-map
   "C-j" 'company-select-next
   "C-n" 'company-select-next
   "C-k" 'company-select-previous
   "C-p" 'company-select-previous
   "C-d" 'company-show-doc-buffer)
  :config
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 2)
  (global-company-mode))

(provide 'wpc-company)
;;; company.el ends here
