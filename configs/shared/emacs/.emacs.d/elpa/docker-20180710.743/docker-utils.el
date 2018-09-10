;;; docker-utils.el --- Random utilities  -*- lexical-binding: t -*-

;; Author: Philippe Vaucher <philippe.vaucher@gmail.com>

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;; Code:

(defun docker-utils-get-marked-items ()
  "Get the marked items data from `tabulated-list-entries'."
  (save-excursion
    (goto-char (point-min))
    (let ((selection ()))
      (while (not (eobp))
        (when (not (null (tablist-get-mark-state)))
          (setq selection (-snoc selection (cons (tabulated-list-get-id) (tabulated-list-get-entry)))))
        (forward-line))
      selection)))

(defun docker-utils-get-marked-items-ids ()
  "Get the id part of `docker-utils-get-marked-items'."
  (-map #'car (docker-utils-get-marked-items)))

(defun docker-utils-setup-popup (val def)
  (magit-with-pre-popup-buffer (docker-utils-select-if-empty))
  (magit-popup-default-setup val def))

(defun docker-utils-select-if-empty (&optional arg)
  "Select current row is selection is empty.
ARG is unused here, but is required by `add-function'."
  (save-excursion
    (when (null (docker-utils-get-marked-items))
      (tablist-put-mark))))

(defun docker-utils-pop-to-buffer (name)
  "Like `pop-to-buffer', but suffix NAME with the host if on a remote host."
  (pop-to-buffer
   (if (file-remote-p default-directory)
       (with-parsed-tramp-file-name default-directory nil (concat name " - " host))
     name)))

(defmacro docker-utils-with-buffer (name &rest body)
  "Wrapper around `with-current-buffer'.
Execute BODY in a buffer."
  (declare (indent defun))
  `(with-current-buffer (generate-new-buffer (format "* docker - %s *" ,name))
     (setq buffer-read-only nil)
     (erase-buffer)
     ,@body
     (setq buffer-read-only t)
     (goto-char (point-min))
     (pop-to-buffer (current-buffer))))

(provide 'docker-utils)

;;; docker-utils.el ends here
