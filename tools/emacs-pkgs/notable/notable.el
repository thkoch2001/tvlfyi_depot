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
(require 'dottime)
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
    (dolist (file (f-entries notable-note-dir))
      (when-let* ((match (string-match notable--note-regexp file))
                  (id (string-to-number
                       (match-string 1 file)))
                  (larger (> id next)))
        (setq next id)))
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

(defun notable--note-path (id)
  (check-type id integer)
  (f-join notable-note-dir (format "note-%d.json" id)))

(defun notable--archive-path (id)
  (check-type id integer)
  (f-join notable-note-dir (format "archive-%d.json" id)))

(defun notable--add-note (content)
  "Add a note with CONTENT to the note store."
  (let* ((id (notable--next-id))
         (note (notable--make-note :time (time-convert nil 'integer)
                                   :content content))
         (path (notable--note-path id)))
    (when (f-exists? path) (error "Note file '%s' already exists!" path))
    (f-write-text (notable--serialize-note note) 'utf-8 path)
    (message "Saved note %d" id)))

(defun notable--archive-note (id)
  "Archive the note with ID."
  (check-type id integer)

  (unless (f-exists? (notable--note-path id))
    (error "There is no note with ID %d." id))

  (when (f-exists? (notable--archive-path id))
    (error "Oh no, a note with ID %d has already been archived!" id))

  (f-move (notable--note-path id) (notable--archive-path id))
  (message "Archived note with ID %d." id))

(defun notable--list-note-ids ()
  "List all note IDs (not contents) from `notable-note-dir'"
  (cl-loop for file in (f-entries notable-note-dir)
           with res = nil
           if (string-match notable--note-regexp file)
           do (push (string-to-number (match-string 1 file)) res)
           finally return res))

(defun notable--get-note (id)
  (let ((path (notable--note-path id)))
    (unless (f-exists? path)
      (error "No note with ID %s in note storage!" id))
    (notable--deserialize-note (f-read-text path 'utf-8))))

;; Note view buffer implementation

(defvar-local notable--buffer-note nil "The note ID displayed by this buffer.")

(define-derived-mode notable-note-mode fundamental-mode "notable-note"
  "Major mode displaying a single Notable note."
  (set (make-local-variable 'scroll-preserve-screen-position) t)
  (setq truncate-lines t)
  (setq buffer-read-only t)
  (setq buffer-undo-list t))

(setq notable-note-mode-map
      (let ((map (make-sparse-keymap)))
        (define-key map "q" 'kill-current-buffer)
        map))

(defun notable--show-note (id)
  "Display a single note in a separate buffer."
  (check-type id integer)

  (let ((note (notable--get-note id))
        (buffer (get-buffer-create (format "*notable: %d*" id)))
        (inhibit-read-only t))
    (with-current-buffer buffer
      (notable-note-mode)
      (erase-buffer)
      (setq notable--buffer-note id)
      (setq header-line-format
            (format "Note from %s"
                    (dottime-format
                     (seconds-to-time (notable--note-time note))))))
    (switch-to-buffer buffer)
    (goto-char (point-min))
    (insert (notable--note-content note))))

(defun notable--show-note-at-point ()
  (interactive)
  (notable--show-note (get-text-property (point) 'notable-note-id)))

(defun notable--archive-note-at-point ()
  (interactive)
  (notable--archive-note (get-text-property (point) 'notable-note-id)))

;; Note list buffer implementation

(define-derived-mode notable-list-mode fundamental-mode "notable"
  "Major mode displaying the Notable note list."
  ;; TODO(tazjin): `imenu' functions?

  (set (make-local-variable 'scroll-preserve-screen-position) t)
  (setq truncate-lines t)
  (setq buffer-read-only t)
  (setq buffer-undo-list t)
  (hl-line-mode t))

(setq notable-list-mode-map
      (let ((map (make-sparse-keymap)))
        (define-key map "a" 'notable--archive-note-at-point)
        (define-key map "q" 'kill-current-buffer)
        (define-key map "g" 'notable-list-notes)
        (define-key map (kbd "RET") 'notable--show-note-at-point)
        map))

(defun notable--render-note (id note)
  (check-type id integer)
  (check-type note notable--note)

  (let* ((start (point))
         (date (dottime-format (seconds-to-time
                                (notable--note-time note))))
         (first-line (truncate-string-to-width
                      (car (s-lines (notable--note-content note)))
                      ;; Length of the window, minus the date prefix:
                      (- (window-width) (+ 2 (length date)))
                      nil nil 1)))
    (insert (propertize (s-concat date "  " first-line)
                        'notable-note-id id))
    (insert "\n")))

(defun notable--render-notes (notes)
  "Retrieve each note in NOTES by ID and insert its contents into
the list buffer.

For larger notes only the first line is displayed."
  (dolist (id notes)
    (notable--render-note id (notable--get-note id))))

;; User-facing functions

(defun notable-take-note (content)
  "Interactively prompt the user for a note that should be stored
in Notable."
  (interactive "sEnter note: ")
  (check-type content string)
  (notable--add-note content))

(defun notable-list-notes ()
  "Open a buffer listing all active notes."
  (interactive)

  (let ((buffer (get-buffer-create "*notable*"))
        (notes (notable--list-note-ids))
        (inhibit-read-only t))
    (with-current-buffer buffer
      (notable-list-mode)
      (erase-buffer)
      (setq header-line-format "Notable notes"))
    (switch-to-buffer buffer)
    (goto-char (point-min))
    (notable--render-notes notes)))

(provide 'notable)
