;;; wpc-python.el --- Hosts python tooling preferences -*- lexical-binding: t -*-
;; Author: William Carroll <wpcarro@gmail.com>

;;; Commentary:
;; Python tooling for work, life, etc.

;;; Code:
(use-package lsp-python
  :config
  (general-add-hook 'python-mode-hook #'lsp-python-enable)
  (general-add-hook 'before-save-hook #'lsp-format-buffer))

(provide 'wpc-python)
;;; wpc-python.el ends here
