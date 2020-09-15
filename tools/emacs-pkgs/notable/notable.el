;;; notable.el --- a simple note-taking app -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 The TVL Contributors
;;
;; Author: Vincent Ambo <mail@tazj.in>
;; Version: 1.0
;; Package-Requires: (cl-lib dash f rx s subr-x)
;;
;;; Commentary:
;;
;; This package provides a simple note-taking application which can be
;; invoked from anywhere in Emacs, with several interactive
;; note-taking functions included.
;;
;; As is tradition for my software, the idea here is to reduce
;; friction which I see even with tools like `org-capture', because
;; `org-mode' does a ton of things I don't care about.
;;
;; Notable stores its notes in simple JSON files in the folder
;; specified by `notable-note-dir'.

(require 'cl-lib)
(require 'dash)
(require 'f)
(require 'ht)
(require 'rx)
(require 's)
(require 'subr-x)

;; User-facing customisation options

(defgroup notable nil
  "Simple note-taking application."
  :group 'applications)

;; TODO(tazjin): Use whatever the XDG state dir thing is for these by
;; default.
(defcustom notable-note-dir (expand-file-name "~/.notable/")
  "File path to the directory containing notable's notes."
  :type 'string
  :group 'notable)

;; Package internal definitions

(cl-defstruct (notable--note (:constructor notable--make-note))
  "Structure containing the fields of a single notable note."
  time    ;; UNIX timestamp at which the note was taken
  content ;; Textual content of the note
  )

(defvar notable--note-lock (make-mutex "notable-notes")
  "Exclusive lock for note operations with shared state.")

(defvar notable--note-regexp
  (rx "note-"
      (group (one-or-more (any num)))
      ".json")
  "Regular expression to match note file names.")

(defvar notable--next-note
  (let ((next 0))
    (-each (f-entries notable-note-dir)
      (lambda (file)
        (when-let* ((match (string-match notable--note-regexp file))
                    (id (string-to-number
                         (match-string 1 file)))
                    (larger (> id next)))
          (setq next id))))
    (+ 1 next))
  "Next ID to use for notes. Initial value is determined based on
  the existing notes files.")

(defun notable--serialize-note (note)
  "Serialise NOTE into JSON format."
  (check-type note notable--note)
  (json-serialize (ht ("time" (notable--note-time note))
                      ("content" (notable--note-content note)))))

(defun notable--deserialize-note (json)
  "Deserialise JSON into a notable note."
  (check-type json string)
  (let ((parsed (json-parse-string json)))
    (unless (and (ht-contains? parsed "time")
                 (ht-contains-p parsed "content"))
      (error "Missing required keys in note structure!"))
    (notable--make-note :time (ht-get parsed "time")
                        :content (ht-get parsed "content"))))

(defun notable--next-id ()
  "Return the next note ID and increment the counter."
  (with-mutex notable--note-lock
    (let ((id notable--next-note))
      (setq notable--next-note (+ 1 id))
      id)))

(defun notable--add-note (content)
  "Add a note with CONTENT to the note store."
  (let* ((id (notable--next-id))
         (note (notable--make-note :time (time-convert nil 'integer)
                                   :content content))
         (path (f-join notable-note-dir (format "note-%d.json" id))))
    (when (f-exists? path) (error "Note file '%s' already exists!" path))
    (f-write-text (notable--serialize-note note) 'utf-8 path)
    (message "Saved note %d" id)))

(defun notable--list-note-ids ()
  "List all note IDs (not contents) from `notable-note-dir'"
  (cl-loop for file in (f-entries notable-note-dir)
           with res = nil
           if (string-match notable--note-regexp file)
           do (push (string-to-number (match-string 1 file)) res)
           finally return res))

;; User-facing functions

(defun notable-take-note (content)
  "Interactively prompt the user for a note that should be stored
in Notable."
  (interactive "sEnter note: ")
  (check-type content string)
  (notable--add-note content))

(provide 'notable)
