;;; variables.el --- Helpful variables for making my ELisp life more enjoyable -*- lexical-binding: t -*-
;; Authpr: William Carroll <wpcarro@gmail.com>

;;; Commentary:
;; This file contains helpful variables that I use in my ELisp development.

;;; Code:

(defconst wpc/current-project
  "~/urbint/grid/"
  "Variable holding the directory for my currently active project.")

(defvar wpc/mouse-kbds
  '([mouse-1] [down-mouse-1] [drag-mouse-1] [double-mouse-1] [triple-mouse-1]
    [mouse-2] [down-mouse-2] [drag-mouse-2] [double-mouse-2] [triple-mouse-2]
    [mouse-3] [down-mouse-3] [drag-mouse-3] [double-mouse-3] [triple-mouse-3]
    [mouse-4] [down-mouse-4] [drag-mouse-4] [double-mouse-4] [triple-mouse-4]
    [mouse-5] [down-mouse-5] [drag-mouse-5] [double-mouse-5] [triple-mouse-5])
  "This variable stores all of the mouse-related keybindings that Emacs recognizes.")

(provide 'variables)
;;; variables.el ends here
