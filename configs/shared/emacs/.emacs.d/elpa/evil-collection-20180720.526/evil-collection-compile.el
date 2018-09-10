;;; evil-collection-compile.el --- Evil bindings for `compile' -*- lexical-binding: t -*-

;; Copyright (C) 2017 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <ambrevar@gmail.com>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1"))
;; Keywords: evil, compile, tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; Evil bindings for `compile'.

;;; Code:
(require 'evil-collection)
(require 'compile)

(defconst evil-collection-compile-maps '(compilation-mode-map))

(defun evil-collection-compile-setup ()
  "Set up `evil' bindings for `compile'."
  (evil-set-initial-state 'compilation-mode 'normal)

  (evil-collection-define-key 'normal 'compilation-mode-map
    "g?" 'describe-mode
    "?" evil-collection-evil-search-backward
    "gg" 'evil-goto-first-line
    "0" 'evil-digit-argument-or-evil-beginning-of-line
    [mouse-2] 'compile-goto-error
    [follow-link] 'mouse-face
    (kbd "<return>") 'compile-goto-error

    "go" 'compilation-display-error
    (kbd "S-<return>") 'compilation-display-error

    "gj" 'compilation-next-error
    "gk" 'compilation-previous-error
    (kbd "C-j") 'compilation-next-error
    (kbd "C-k") 'compilation-previous-error
    "[" 'compilation-previous-file
    "]" 'compilation-next-file
    "gr" 'recompile))

(provide 'evil-collection-compile)
;;; evil-collection-compile.el ends here
