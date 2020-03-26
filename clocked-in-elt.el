;;; ~/.doom.d/clocked-in-elt.el -*- lexical-binding: t; -*-
;;;
(load (expand-file-name "init" (or (getenv "EMACSDIR")
               (expand-file-name
                "../.emacs.d"
                (file-name-directory (file-truename load-file-name))))))

(require 'org-clock)
(require 'org-element)

(let ((item (or org-clock-marker
                (car org-clock-history))))
  (when item
    (with-current-buffer (marker-buffer item)
      (goto-char (marker-position item))
      (let ((element (org-element-at-point)))
        (when (eq 'headline (car element))
          (message "%s" (plist-get (cadr element) :raw-value)))))))
