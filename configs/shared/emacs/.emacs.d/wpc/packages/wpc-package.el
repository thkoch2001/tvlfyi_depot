;;; package.el --- My package configuration -*- lexical-binding: t -*-
;; Author: William Carroll <wpcarro@gmail.com>

;;; Commentary:
;; This module hosts all of the settings required to work with ELPA,
;; MELPA, QUELPA, and co.

;;; Code:

(require 'package)
(package-initialize)

(require 'general)
(require 'use-package)

(add-to-list 'load-path "~/.emacs.d/vendor/")
(add-to-list 'load-path "~/.emacs.d/wpc/")
(add-to-list 'load-path "~/.emacs.d/wpc/packages")

(provide 'wpc-package)
;;; wpc-package.el ends here
