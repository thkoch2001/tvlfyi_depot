;;; passage.el --- Emacs passage support -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023 William Carroll <wpcarro@gmail.com>

;; Author: William Carroll <wpcarro@gmail.com>
;; Version: 1.0.0

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
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

;; This package provides functions for working with passage.

;;; Code:

(require 'dash)
(require 'f)
(require 's)

(defgroup passage nil
  "Customization options for `passage'."
  :prefix "passage-"
  :group 'vterm)

(defcustom passage-store
  "~/.passage/store"
  "Path to the passage store directory."
  :type 'string
  :group 'passage)

(defcustom passage-executable
  (or (executable-find "passage")
      "/nix/store/jgffkfdiiwiqa4zqpxn3691mx9xc6axa-passage-unstable-2022-05-01/bin/passage")
  "Path to passage executable."
  :type 'string
  :group 'passage)

(defun passage-select ()
  "Select an entry and copy its password to the kill ring."
  (interactive)
  (let ((key (completing-read "Copy password of entry: "
                              (-map (lambda (x)
                                      (f-no-ext (f-relative x passage-store)))
                                    (f-files passage-store nil t)))))
    (kill-new
     (s-trim-right
      (shell-command-to-string
       (format "%s show %s | head -1" passage-executable key))))
    (message "[passage.el] Copied \"%s\"!" key)))

(provide 'passage)
;;; passage.el ends here
