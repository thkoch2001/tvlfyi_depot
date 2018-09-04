;;; package.el --- My package configuration -*- lexical-binding: t -*-
;; Author: William Carroll <wpcarro@gmail.com>

;;; Commentary:
;; This module hosts all of the settings required to work with ELPA,
;; MELPA, QUELPA, and co.

;;; Code:

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/") t)
(package-initialize)

(if (require 'quelpa nil t)
    (quelpa-self-upgrade)
  (with-temp-buffer
    (url-insert-file-contents "https://raw.github.com/quelpa/quelpa/master/bootstrap.el")
    (eval-buffer)))

(require 'use-package)
(setq use-package-always-ensure t)
;; Remove this line once general integration with use-package calls
;; with-eval-after-load 'use-package-core instead of 'use-package
(use-package general)

(add-to-list 'load-path "~/.emacs.d/vendor/")
(add-to-list 'load-path "~/.emacs.d/wpc/")
(add-to-list 'load-path "~/.emacs.d/wpc/packages")

(provide 'wpc-package)
;;; package.el ends here
