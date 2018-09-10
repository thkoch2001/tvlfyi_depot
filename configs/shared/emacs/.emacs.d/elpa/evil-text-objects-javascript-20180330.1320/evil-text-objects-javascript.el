;;; evil-text-objects-javascript.el --- Text objects for Javascript source code

;; Version: 1.0.0
;; Package-Version: 20180330.1320

;;; License:

;; Copyright (C) 2018 Off Market Data, Inc. DBA Urbint
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to
;; deal in the Software without restriction, including without limitation the
;; rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
;; sell copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
;; IN THE SOFTWARE.

;;; Commentary:

;; evil-text-objects-javascript provides text-object definitions that
;; should make working with Javascript in Emacs more enjoyable.
;;
;; Currently supporting:
;;   - functions
;;
;; See the README.md for installation instructions.

(require 'evil)
(require 'bind-key)

;;; Code:

;; functions
(evil-define-text-object
  evil-inner-javascript-function (count &optional beg end type)
  "Inner text object for all Javascript functions."
  (call-interactively #'mark-defun)
  (narrow-to-region (region-beginning) (region-end))
  (goto-char (point-min))
  (let* ((beg (save-excursion
                (search-forward "(")
                (backward-char)
                (evil-jump-item)
                (search-forward-regexp "[({]")
                (point)))
         (end (save-excursion
                (goto-char beg)
                (evil-jump-item)
                (point))))
    (evil-range beg end type)))

(evil-define-text-object
  evil-outer-javascript-function (count &optional beg end type)
  "Outer text object for all Javascript functions."
  (call-interactively #'mark-defun)
  (narrow-to-region (region-beginning) (region-end))
  (goto-char (point-min))
  (let* ((beg (save-excursion
                (when (looking-at "[[:space:]]")
                  (evil-forward-word-begin))
                (point)))
         (end (save-excursion
                (goto-char beg)
                (search-forward "(")
                (backward-char)
                (evil-jump-item)
                (search-forward-regexp "[({]")
                (evil-jump-item)
                (forward-char)
                (point))))
    (evil-range beg end type)))

;; comments
(evil-define-text-object
  evil-outer-javascript-single-line-comment (count &optional beg end type)
  "Outer text object for a single-line Javascript comment."
  (let ((beg (save-excursion
               (re-search-backward "\/\/[[:space:]]*" (line-beginning-position))
               (when (looking-at "/") (point))))
        (end (save-excursion
               (end-of-line)
               (point))))
    (evil-range beg end type)))

(evil-define-text-object
  evil-inner-javascript-single-line-comment (count &optional beg end type)
  "Inner text object for a single-line Javascript comment."
  (let ((beg (save-excursion
               (re-search-backward "\/\/[[:space:]]*" (line-beginning-position))
               (when (looking-at "/")
                 (progn
                   (evil-forward-char 2)
                   (point)))))
        (end (save-excursion
               (end-of-line)
               (point))))
    (evil-range beg end type)))

(defun etojs--beg-multi-line-comment ()
  "Navigate to the beginning of a multi-line Javascript comment."
  (re-search-backward "\/\\*\\*?"))

(defun etojs--end-multi-line-comment ()
  "Navigate to the end of a multi-line Javascript comment."
  (re-search-forward "\*/"))

(evil-define-text-object
  evil-outer-javascript-multi-line-comment (count &optional beg end type)
  "Outer text object for a multi-line Javascript comment."
  (let ((end (save-excursion
               (etojs--end-multi-line-comment)
               (point)))
        (beg (save-excursion
               (etojs--beg-multi-line-comment)
               (point))))
    (evil-range beg end type)))

(evil-define-text-object
  evil-inner-javascript-multi-line-comment (count &optional beg end type)
  (let ((end (save-excursion
               (etojs--end-multi-line-comment)
               (forward-char -4)
               (point)))
        (beg (save-excursion
               (etojs--beg-multi-line-comment)
               (evil-next-line)
               (evil-first-non-blank)
               (forward-char 2)
               (point))))
    (evil-range beg end type)))

;; Installation Helper
(defun evil-text-objects-javascript/install ()
  "Register keybindings for the text objects defined herein.  It is
recommended to run this after something like `rjsx-mode-hook'.  See
README.md for additional information."
  (bind-keys :map evil-operator-state-local-map
             ("af" . evil-outer-javascript-function)
             ("if" . evil-inner-javascript-function)
             ("ac" . evil-outer-javascript-single-line-comment)
             ("ic" . evil-inner-javascript-single-line-comment)
             ("aC" . evil-outer-javascript-multi-line-comment)
             ("iC" . evil-inner-javascript-multi-line-comment))
  (bind-keys :map evil-visual-state-local-map
             ("af" . evil-outer-javascript-function)
             ("if" . evil-inner-javascript-function)
             ("ac" . evil-outer-javascript-single-line-comment)
             ("ic" . evil-inner-javascript-single-line-comment)
             ("aC" . evil-outer-javascript-multi-line-comment)
             ("iC" . evil-inner-javascript-multi-line-comment)))

(provide 'evil-text-objects-javascript)

;;; evil-text-objects-javascript.el ends here
