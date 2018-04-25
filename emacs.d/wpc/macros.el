;;; macros.el --- Helpful variables for making my ELisp life more enjoyable -*- lexical-binding: t -*-
;; Authpr: William Carroll <wpcarro@gmail.com>

;;; Commentary:
;; This file contains helpful variables that I use in my ELisp development.

;;; Code:

(require 'dash)
(require 's)
(require 'string-functions)

(defmacro xi (&rest FORMS)
  `(lambda ,(--filter (s-contains? (symbol-name it)
                                   (prin1-to-string FORMS))
                      '(x1 x2 x3 x4 x5))
     ,FORMS))

(defmacro enable (mode)
  "Helper for enabling MODE. Useful in `add-hook' calls."
  `#'(lambda nil (,mode 1)))

(defmacro disable (mode)
  "Helper for disabling MODE. Useful in `add-hook' calls."
  `#'(lambda nil (,mode -1)))

(defmacro add-hooks (modes)
  "Add multiple MODES for the CALLBACK."
  `(dolist (mode ,modes)
     (add-hook (symbol/ensure-hookified mode) ,callback)))

(provide 'macros)
;;; macros.el ends here
