;;; dired.el --- My dired preferences -*- lexical-binding: t -*-
;; Author: William Carroll <wpcarro@gmail.com>

;;; Commentary:
;; Hosts my attempts at configuring dired

;;; Code:

(require 'dired)
(general-def 'dired-mode-map
    "c" 'find-file
    "f" 'wpc/find-file
    "-" 'dired-up-directory)
(general-add-hook 'dired-mode-hook (list (enable dired-hide-details-mode)
                                         #'auto-revert-mode))

(provide 'wpc-dired)
;;; dired.el ends here
