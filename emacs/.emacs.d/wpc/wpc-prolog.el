;;; wpc-prolog.el --- For Prologging things -*- lexical-binding: t -*-
;; Author: William Carroll <wpcarro@gmail.com>

;;; Commentary:
;; Code configuring my Prolog work.

;;; Code:

(require 'macros)

;; TODO: Notice that the .pl extension conflicts with Perl files. This may
;; become a problem should I start working with Perl.
(macros/support-file-extension "pl" prolog-mode)

(provide 'wpc-prolog)
;;; wpc-prolog.el ends here
