;;; wpc-python.el --- Python configuration -*- lexical-binding: t -*-
;; Author: William Carroll <wpcarro@gmail.com>

;;; Commentary:
;; My Python configuration settings
;;
;; Depends
;; - `apti yapf`

;;; Code:

(use-package py-yapf
  :config
  (add-hook 'python-mode-hook #'py-yapf-enable-on-save))

(provide 'wpc-python)
;;; wpc-python.el ends here
