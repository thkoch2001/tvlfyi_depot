;;; wpc-elixir.el --- Elixir / Erland configuration -*- lexical-binding: t -*-
;; Author: William Carroll <wpcarro@gmail.com>

;;; Commentary:
;; My preferences for working with Elixir / Erlang projects

;;; Code:
(use-package elixir-mode
  :config
  (general-add-hook 'elixir-mode-hook
                    (lambda ()
                      (add-hook 'before-save-hook #'elixir-format nil t))))

(provide 'wpc-elixir)
;;; wpc-elixir.el ends here
