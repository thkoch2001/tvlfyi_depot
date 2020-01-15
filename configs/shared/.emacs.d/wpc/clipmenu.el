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
  (or (getenv "CM_HISTLENGTH") 20)
  "Limit the number of clips in the history.
This value defaults to 20.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: Ensure the clips are sorted in a LRU order.
;; TODO: Ensure entries are deduped.
;; TODO: Ensure multiline entries can be handled.
(defun clipmenu/list-clips ()
  "Return a list of the content of all of the clips."
  (->> clipmenu/cache-file-pattern
       f-glob
       (-map (lambda (path)
               (->> path f-read (s-split "\n"))))
       -flatten
       (-sort (lambda (a b)
                (> (->> a (s-split " ") (nth 0) string-to-number)
                   (->> b (s-split " ") (nth 0) string-to-number))))
       (-map (lambda (entry)
               (->> entry (s-split " ") (nth 1))))
       ;; TODO: Here we should actually only be deleting adjacent
       ;; duplicates. This will be both faster and more similar to the behavior
       ;; of the clipmenu program the author wrote.
       delete-dups
       (-take clipmenu/history-length)))

;; TODO: Add tests.
(defun clipmenu/escape-quotes (x)
  "Escape double and single quotes in X."
  (->> x
       (s-replace "\"" "\\\"")
       (s-replace "'" "\\'")))

;; TODO: Properly handle errors when the file doesn't exist.
(defun clipmenu/line-to-clip (line)
  "Map the chosen LINE to a clip stored on disk."
  (->> line
       clipmenu/cksum
       (f-join clipmenu/cache-directory)
       f-read
       clipboard/copy))

;; TODO: Consider supporting :history keyword.
;; TODO: Ensure ivy maintains the sort from `clipmenu/list-clips'.
;; TODO: Ensure you can handle special characters like:
;; r}_rh,pmj~kCR.<5w"PUk#Z^>.
;; TODO: Consider adding tests.
(defun clipmenu/ivy-copy ()
  "Use `ivy-read' to select and copy a clip."
  (interactive)
  (ivy-read "Clipmenu: "
            (clipmenu/list-clips)
            :action #'clipmenu/line-to-clip))

;; TODO: Delete this once `clipmenu/ivy-copy' is working as expected.
;; TODO: Use this to compare behavior with `clipmenu/ivy-copy'. These functions
;; should behave in almost exactly the same way.
(defun clipmenu/dmenu-copy ()
  "Call clipmenu with dmenu as the client."
  (interactive)
  (prelude/start-process
   :name "clipboard/select"
   :command "clipmenu"))

;; TODO: Write a faster alternative because this currently takes 1/2s to run,
;; which is ridiculous. Perhaps `call-process' is what we need.
(defun clipmenu/cksum (content)
  "Return the CRC checksum of CONTENT."
  (->> (shell-command-to-string
        (format "zsh -c 'cksum <<<\"%s\"'"
                ;; TODO: I'm not sure this is working as intended.
                (clipmenu/escape-quotes content)))
       s-trim-right))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keybindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when clipmenu/install-kbds?
  ;; TODO: Delete this once `clipmenu/ivy-copy' is working as expected.
  (exwm-input-set-key
   (kbd "C-M-S-v") #'clipmenu/dmenu-copy)
  (exwm-input-set-key
   (kbd "C-M-v") #'clipmenu/ivy-copy))

(provide 'clipmenu)
;;; clipmenu.el ends here
