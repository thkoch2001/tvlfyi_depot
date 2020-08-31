;;; region.el --- Functions for working with regions -*- lexical-binding: t -*-

;; Author: William Carroll <wpcarro@gmail.com>
;; Version: 0.0.1
;; Package-Requires: ((emacs "24"))
;; Homepage: https://user.git.corp.google.com/wpcarro/briefcase

;;; Commentary:
;; Sometimes Emacs's function names and argument ordering is great; other times,
;; it isn't.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Library
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun region-to-string ()
  "Returns the string in the active region."
  (buffer-substring-no-properties (region-beginning)
                                  (region-end)))

(provide 'region)
;;; region.el ends here
