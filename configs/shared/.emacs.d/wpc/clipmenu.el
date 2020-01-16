;;; clipmenu.el --- Emacs client for clipmenu -*- lexical-binding: t -*-
;; Author: William Carroll <wpcarro@gmail.com>

;;; Commentary:
;; Ivy integration with the excellent program, clipmenu.
;;
;; clipmenu is a simple clipboard manager xsel. Usually clipmenu integrates with
;; dmenu.  This Emacs module integrates with ivy.  Launch this when you want to
;; select a clip.
;;
;; The following environment variables allow you to customize clipmenu's
;; behavior:
;;
;; - CM_DIR: specify the base directory to store the cache dir in
;;   (default: $XDG_RUNTIME_DIR, $TMPDIR, or /tmp)
;; - CM_HISTLENGTH: specify the number of lines to show in ivy. (default: 8)
;;
;; Other variables for customizing clipmenu are defined herein.
;;
;; For more information, see `clipmenu --help`.

;;; Code:

;; TODO: Support an ivy action of deleting an entry from clipmenu.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dependencies
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'f)
(require 's)
(require 'dash)

(prelude/assert
 (prelude/executable-exists? "clipmenud"))

(prelude/assert
 (prelude/executable-exists? "clipmenu"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: Remove this if you're publishing it.
(defcustom clipmenu/install-kbds? t
  "When t, install the keybindings defined herein")

(defcustom clipmenu/directory
  (or (getenv "XDG_RUNTIME_DIR")
      (getenv "TMPDIR")
      "/tmp")
  "Base directory for clipmenu data.")

(defconst clipmenu/major-version 5
   "The major version number for clipmenu.")

(defconst clipmenu/cache-directory
  (f-join clipmenu/directory
          (format "clipmenu.%s.%s"
                  clipmenu/major-version
                  (getenv "USER")))
  "Directory where the clips are stored.")

(defconst clipmenu/cache-file-pattern
  (f-join clipmenu/cache-directory "line_cache_*")
  "Glob pattern matching the locations on disk for clipmenu's labels.")

(defcustom clipmenu/history-length
  (or (getenv "CM_HISTLENGTH") 50)
  "Limit the number of clips in the history.
This value defaults to 50.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun clipmenu/parse-line (x)
  "Parse the entry in the clipmenu's line-cache."
  (string-to-number
   (list/join "" (parsec-with-input x (parsec-count 19 (parsec-digit))))))

(defun clipmenu/parse-content (x)
  "Parse the label from the entry in clipmenu's line-cache."
  (list/join
   ""
   (parsec-with-input x
     (parsec-count 19 (parsec-digit))
     (parsec-str " ")
     (parsec-many (parsec-any-ch)))))

(defun clipmenu/list-clips ()
  "Return a list of the content of all of the clips."
  (->> clipmenu/cache-file-pattern
       f-glob
       (-map (lambda (path)
               (s-split "\n" (f-read path) t)))
       -flatten
       (-reject #'s-blank?)
       (-sort (lambda (a b)
                (< (clipmenu/parse-line a)
                   (clipmenu/parse-line b))))
       (-map #'clipmenu/parse-content)
       list/dedupe-adjacent
       (-take clipmenu/history-length)))

;; TODO: Add tests.
(defun clipmenu/escape-quotes (x)
  "Escape double and single quotes in X."
  (->> x
       (s-replace "\"" "\\\"")
       (s-replace "'" "\\'")))

(defun clipmenu/line-to-clip (line)
  "Map the chosen LINE to a clip stored on disk."
  (->> line
       clipmenu/checksum
       (f-join clipmenu/cache-directory)
       f-read
       clipboard/copy))

;; TODO: Consider supporting :history keyword.

;; TODO: Ensure you can handle special characters like:
;; r}_rh,pmj~kCR.<5w"PUk#Z^>.

(defun clipmenu/ivy-copy ()
  "Use `ivy-read' to select and copy a clip."
  (interactive)
  (let ((ivy-sort-functions-alist nil))
    (ivy-read "Clipmenu: "
              (clipmenu/list-clips)
              :action #'clipmenu/line-to-clip)))

(defun clipmenu/checksum (content)
  "Return the CRC checksum of CONTENT."
  (s-trim-right
   (prelude/call-process-to-string
    "/bin/bash" "-c" (string/format "/usr/bin/cksum <<<'%s'" content))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keybindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when clipmenu/install-kbds?
  (exwm-input-set-key
   (kbd "C-M-v") #'clipmenu/ivy-copy))

(provide 'clipmenu)
;;; clipmenu.el ends here
