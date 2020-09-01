;;; ivy-clipmenu.el --- Emacs client for clipmenu -*- lexical-binding: t -*-

;; Author: William Carroll <wpcarro@gmail.com>
;; Version: 0.0.1
;; URL: https://git.wpcarro.dev/wpcarro/briefcase
;; Package-Requires: ((emacs "25.1"))

;;; Commentary:
;; Ivy integration with the clipboard manager, clipmenu.  Essentially, clipmenu
;; turns your system clipboard into a list.
;;
;; To use this module, you must first install clipmenu and ensure that the
;; clipmenud daemon is running.  Refer to the installation instructions at
;; github.com/cdown/clipmenu for those details.
;;
;; This module intentionally does not define any keybindings since I'd prefer
;; not to presume my users' preferences.  Personally, I use EXWM as my window
;; manager, so I call `exwm-input-set-key' and map it to `ivy-clipmenu-copy'.
;;
;; Usually clipmenu integrates with rofi or dmenu.  This Emacs module integrates
;; with ivy.  Launch this when you want to select a clip.
;;
;; Clipmenu itself supports a variety of environment variables that allow you to
;; customize its behavior.  These variables are respected herein.  If you'd
;; prefer to customize clipmenu's behavior from within Emacs, refer to the
;; variables defined in this module.
;;
;; For more information:
;; - See `clipmenu --help`.
;; - Visit github.com/cdown/clipmenu.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dependencies
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'f)
(require 's)
(require 'dash)
(require 'ivy)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgroup ivy-clipmenu nil
  "Ivy integration for clipmenu."
  :group 'ivy)

(defcustom ivy-clipmenu-directory
  (or (getenv "XDG_RUNTIME_DIR")
      (getenv "TMPDIR")
      "/tmp")
  "Base directory for clipmenu's data."
  :type 'string
  :group 'ivy-clipmenu)

(defconst ivy-clipmenu-executable-version 5
   "The major version number for the clipmenu executable.")

(defconst ivy-clipmenu-cache-directory
  (f-join ivy-clipmenu-directory
          (format "clipmenu.%s.%s"
                  ivy-clipmenu-executable-version
                  (getenv "USER")))
  "Directory where the clips are stored.")

(defconst ivy-clipmenu-cache-file-pattern
  (f-join ivy-clipmenu-cache-directory "line_cache_*")
  "Glob pattern matching the locations on disk for clipmenu's labels.")

(defcustom ivy-clipmenu-history-length
  (or (getenv "CM_HISTLENGTH") 25)
  "Limit the number of clips in the history.
This value defaults to 25.")

(defvar ivy-clipmenu-history nil
  "History for `ivy-clipmenu-copy'.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ivy-clipmenu-parse-content (x)
  "Parse the label from the entry, X, in clipmenu's line-cache."
  (->> (s-split " " x)
       (-drop 1)
       (s-join " ")))

(defun ivy-clipmenu-list-clips ()
  "Return a list of the content of all of the clips."
  (->> ivy-clipmenu-cache-file-pattern
       f-glob
       (-map (lambda (path)
               (s-split "\n" (f-read path) t)))
       -flatten
       (-reject #'s-blank?)
       (-sort #'string>)
       (-map #'ivy-clipmenu-parse-content)
       delete-dups
       (-take ivy-clipmenu-history-length)))

(defun ivy-clipmenu-checksum (content)
  "Return the CRC checksum of CONTENT."
  (s-trim-right
   (with-temp-buffer
     (call-process "/bin/bash" nil (current-buffer) nil "-c"
                   (format "cksum <<<'%s'" content))
     (buffer-string))))

(defun ivy-clipmenu-line-to-content (line)
  "Map the chosen LINE from the line cache its content from disk."
  (->> line
       ivy-clipmenu-checksum
       (f-join ivy-clipmenu-cache-directory)
       f-read))

(defun ivy-clipmenu-do-copy (x)
  "Copy string, X, to the system clipboard."
  (kill-new x)
  (message "[ivy-clipmenu.el] Copied!"))

(defun ivy-clipmenu-copy ()
  "Use `ivy-read' to select and copy a clip.
It's recommended to bind this function to a globally available keymap."
  (interactive)
  (let ((ivy-sort-functions-alist nil))
    (ivy-read "Clipmenu: "
              (ivy-clipmenu-list-clips)
              :history 'ivy-clipmenu-history
              :action (lambda (line)
                        (->> line
                             ivy-clipmenu-line-to-content
                             ivy-clipmenu-do-copy)))))

(provide 'ivy-clipmenu)
;;; ivy-clipmenu.el ends here
