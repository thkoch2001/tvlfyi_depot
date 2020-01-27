;;; constants.el --- Constants for organizing my Emacs -*- lexical-binding: t -*-
;; Authpr: William Carroll <wpcarro@gmail.com>

;;; Commentary:
;; This file contains constants that are shared across my configuration.

;;; Code:

;; TODO: Consider merging `ui.el' and `misc.el' because those are the only
;; current consumers of these constants, and I'm unsure if the indirection that
;; globally defined constants introduces is worth it.

(defconst constants/current-project "~/universe"
  "Variable holding the directory for my currently active project.")

(prelude/assert (f-directory? constants/current-project))

(defconst constants/mouse-kbds
  '([mouse-1] [down-mouse-1] [drag-mouse-1] [double-mouse-1] [triple-mouse-1]
    [mouse-2] [down-mouse-2] [drag-mouse-2] [double-mouse-2] [triple-mouse-2]
    [mouse-3] [down-mouse-3] [drag-mouse-3] [double-mouse-3] [triple-mouse-3]
    [mouse-4] [down-mouse-4] [drag-mouse-4] [double-mouse-4] [triple-mouse-4]
    [mouse-5] [down-mouse-5] [drag-mouse-5] [double-mouse-5] [triple-mouse-5])
  "All of the mouse-related keybindings that Emacs recognizes.")

(defconst constants/fill-column 80
  "Variable used to set the defaults for wrapping, highlighting, etc.")

(provide 'constants)
;;; constants.el ends here
