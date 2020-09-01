;;; constants.el --- Constants for organizing my Emacs -*- lexical-binding: t -*-

;; Author: William Carroll <wpcarro@gmail.com>
;; Version: 0.0.1
;; URL: https://git.wpcarro.dev/wpcarro/briefcase
;; Package-Requires: ((emacs "24"))

;;; Commentary:
;; This file contains constants that are shared across my configuration.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dependencies
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'prelude)
(require 'f)
(require 'maybe)

(prelude-assert (f-exists? (getenv "BRIEFCASE")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst constants-ci?
  (maybe-some? (getenv "CI"))
  "True when Emacs is running in CI.")

(defconst constants-briefcase
  (getenv "BRIEFCASE")
  "Path to my monorepo, which various parts of my configuration rely on.")

;; TODO: Consider merging `ui.el' and `misc.el' because those are the only
;; current consumers of these constants, and I'm unsure if the indirection that
;; globally defined constants introduces is worth it.

(defconst constants-current-project
  constants-briefcase
  "Variable holding the directory for my currently active project.")

(defconst constants-mouse-kbds
  '([mouse-1] [down-mouse-1] [drag-mouse-1] [double-mouse-1] [triple-mouse-1]
    [mouse-2] [down-mouse-2] [drag-mouse-2] [double-mouse-2] [triple-mouse-2]
    [mouse-3] [down-mouse-3] [drag-mouse-3] [double-mouse-3] [triple-mouse-3]
    [mouse-4] [down-mouse-4] [drag-mouse-4] [double-mouse-4] [triple-mouse-4]
    [mouse-5] [down-mouse-5] [drag-mouse-5] [double-mouse-5] [triple-mouse-5])
  "All of the mouse-related keybindings that Emacs recognizes.")

(defconst constants-fill-column 80
  "Variable used to set the defaults for wrapping, highlighting, etc.")

(provide 'constants)
;;; constants.el ends here
