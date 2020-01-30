;;; package.el --- My package configuration -*- lexical-binding: t -*-
;; Author: William Carroll <wpcarro@gmail.com>

;;; Commentary:
;; This module hosts all of the settings required to work with ELPA,
;; MELPA, QUELPA, and co.

;;; Code:

(require 'package)

;; Even though we're packaging our Emacs with Nix, having MELPA registered is
;; helpful to ad-hoc test out packages before declaratively adding them to
;; emacs/default.nix.
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  ;; TODO: Consider removing this to improve initialization speed.
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))
(use-package general)

(provide 'wpc-package)
;;; wpc-package.el ends here
