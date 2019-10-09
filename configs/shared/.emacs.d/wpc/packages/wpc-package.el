;;; package.el --- My package configuration -*- lexical-binding: t -*-
;; Author: William Carroll <wpcarro@gmail.com>

;;; Commentary:
;; This module hosts all of the settings required to work with ELPA,
;; MELPA, QUELPA, and co.

;;; Code:

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))
(setq use-package-always-ensure t)
(use-package general)

(add-to-list 'load-path "~/.emacs.d/vendor/")
(add-to-list 'load-path "~/.emacs.d/wpc/")
(add-to-list 'load-path "~/.emacs.d/wpc/packages")

(provide 'wpc-package)
;;; wpc-package.el ends here
