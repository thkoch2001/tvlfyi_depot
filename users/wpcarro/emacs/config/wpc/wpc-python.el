;;; wpc-python.el --- Python configuration -*- lexical-binding: t -*-

;; Author: William Carroll <wpcarro@gmail.com>
;; Version: 0.0.1
;; Package-Requires: ((emacs "24"))

;;; Commentary:
;; My Python configuration settings
;;
;; Depends
;; - `apti yapf`

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package py-yapf
  :config
  (add-hook 'python-mode-hook #'py-yapf-enable-on-save))

(provide 'wpc-python)
;;; wpc-python.el ends here
