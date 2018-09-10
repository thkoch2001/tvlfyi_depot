;;; fs-functions.el --- Functions to make working with the filesystem easier. -*- lexical-binding: t -*-
;; Author: William Carroll <wpcarro@gmail.com>

;;; Commentary:
;; Hosts ergonomic functions for working with a filesystem.

;;; Code:

(require 'dash)
(require 'f)

(defun ensure-file-path (path)
  "Ensure that a file and its directories in PATH exist.
Will error for inputs with a trailing slash."
  (when (s-ends-with? "/" path)
    (error (format "Input path has trailing slash: %s" path)))
  (let ((dirs (->> path f-dirname f-split)))
    (apply #'f-mkdir dirs)
    (f-touch path)))

(defun ensure-dir-path (path)
  "Ensures that a directory and its ancestor directories in PATH exist."
  (->> path
       f-split
       (apply #'f-mkdir)))

(provide 'fs-functions)
;;; fs-functions.el ends here
