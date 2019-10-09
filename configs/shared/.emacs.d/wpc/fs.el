;;; fs.el --- Make working with the filesystem easier -*- lexical-binding: t -*-
;; Author: William Carroll <wpcarro@gmail.com>

;;; Commentary:
;; Ergonomic alternatives for working with the filesystem.

;; Dependencies
(require 'f)

;;; Code:

(defun fs/ensure-file (path)
  "Ensure that a file and its directories in `PATH' exist.
Will error for inputs with a trailing slash."
  (when (s-ends-with? "/" path)
    (error (format "Input path has trailing slash: %s" path)))
  (->> path
       f-dirname
       fs/ensure-dir)
  (f-touch path))

(f-dirname "/tmp/a/b/file.txt")

(defun fs/ensure-dir (path)
  "Ensure that a directory and its ancestor directories in `PATH' exist."
  (->> path
       f-split
       (apply #'f-mkdir)))

(defun fs/ls (dir &optional full-path?)
  "List the files in `DIR' one-level deep.
Should behave similarly in spirit to the Unix command, ls.
If `FULL-PATH?' is set, return the full-path of the files."
  (-drop 2 (directory-files dir full-path?)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: Support `refute' function / macro.
(ert-deftest fs/test/ensure-file ()
  (let ((file "/tmp/file/a/b/c/file.txt"))
    ;; Ensure this file doesn't exist first to prevent false-positives.
    (f-delete file t)
    (fs/ensure-file file)
    (should (and (f-exists? file)
                 (f-file? file)))))

(ert-deftest fs/test/ensure-dir ()
  (let ((dir "/tmp/dir/a/b/c"))
    ;; Ensure the directory doesn't exist.
    (f-delete dir t)
    (fs/ensure-dir dir)
    (should (and (f-exists? dir)
                 (f-dir? dir)))))

(provide 'fs)
;;; fs.el ends here
