;;; dired.el --- My dired preferences -*- lexical-binding: t -*-
;; Author: William Carroll <wpcarro@gmail.com>

;;; Commentary:
;; Hosts my attempts at configuring dired

;;; Code:

(require 'dired)
(general-nmap
  :keymaps 'dired-mode-map
  "c" #'find-file
  "f" #'wpc/find-file
  "-" #'dired-up-directory)
(general-unbind
  :keymaps 'dired-mode-map
  "s")
(general-add-hook 'dired-mode-hook (list (enable dired-hide-details-mode)
                                         #'auto-revert-mode))

(provide 'wpc-dired)
;;; wpc-dired.el ends here
