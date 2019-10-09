;;; wpc-elixir.el --- Elixir / Erland configuration -*- lexical-binding: t -*-
;; Author: William Carroll <wpcarro@gmail.com>

;;; Commentary:
;; My preferences for working with Elixir / Erlang projects

;;; Code:
(use-package elixir-mode
  :config
  (add-hook-before-save 'elixir-mode-hook #'elixir-format))

(provide 'wpc-elixir)
;;; wpc-elixir.el ends here
