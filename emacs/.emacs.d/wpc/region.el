;;; region.el --- Functions for working with Emacs's regions -*- lexical-binding: t -*-
;; Author: William Carroll <wpcarro@gmail.com>

;;; Commentary:
;; Sometimes Emacs's function names and argument ordering is great; other times,
;; it isn't.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Library
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun region/to-string ()
  "Returns the string in the active region."
  (buffer-substring-no-properties (region-beginning)
                                  (region-end)))

(provide 'region)
;;; region.el ends here
