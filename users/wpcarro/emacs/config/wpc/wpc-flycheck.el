;;; wpc-flycheck.el --- My flycheck configuration -*- lexical-binding: t -*-

;; Author: William Carroll <wpcarro@gmail.com>
;; Version: 0.0.1
;; Package-Requires: ((emacs "24"))

;;; Commentary:
;; Hosts my Flycheck preferences

;;; Code:

(use-package flycheck
  :config
  (global-flycheck-mode))

(provide 'wpc-flycheck)
;;; wpc-flycheck.el ends here
