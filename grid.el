;;; ~/.doom.d/grid.el -*- lexical-binding: t; -*-

(require 's)

(defun grfn/all-match-groups (s)
  (loop for n from 1
        for x = (match-string n s)
        while x
        collect x))

(defun projectile-grid-ff (path &optional ask)
  "Call `find-file' function on PATH when it is not nil and the file exists.
If file does not exist and ASK in not nil it will ask user to proceed."
  (if (or (and path (file-exists-p path))
          (and ask (yes-or-no-p
                    (s-lex-format
                     "File does not exists. Create a new buffer ${path} ?"))))
      (find-file path)))

(defun projectile-grid-goto-file (filepath &optional ask)
  "Find FILEPATH after expanding root.  ASK is passed straight to `projectile-grid-ff'."
  (projectile-grid-ff (projectile-expand-root filepath) ask))

(defun projectile-grid-choices (ds)
  "Uses `projectile-dir-files' function to find files in directories.
The DIRS is list of lists consisting of a directory path and regexp to filter files from that directory.
Optional third element can be present in the DS list. The third element will be a prefix to be placed before
the filename in the resulting choice.
Returns a hash table with keys being short names (choices) and values being relative paths to the files."
  (loop with hash = (make-hash-table :test 'equal)
        for (dir re prefix) in ds do
        (loop for file in (projectile-dir-files (projectile-expand-root dir)) do
              (when (string-match re file)
                (puthash
                 (concat (or prefix "")
                         (s-join "/" (grfn/all-match-groups file)))
                 (concat dir file)
                 hash)))
        finally return hash))

(defmacro projectile-grid-find-resource (prompt dirs &optional newfile-template)
  "Presents files from DIRS with PROMPT to the user using `projectile-completing-read'.
If users chooses a non existant file and NEWFILE-TEMPLATE is not nil
it will use that variable to interpolate the name for the new file.
NEWFILE-TEMPLATE will be the argument for `s-lex-format'.
The bound variable is \"filename\"."
  `(lexical-let ((choices (projectile-grid-choices ,dirs)))
     (projectile-completing-read
      ,prompt
      (hash-table-keys choices)
      :action
      (lambda (c)
        (let* ((filepath (gethash c choices))
               (filename c)) ;; so `s-lex-format' can interpolate FILENAME
          (if filepath
              (projectile-grid-goto-file filepath)
            (when-let ((newfile-template ,newfile-template))
              (projectile-grid-goto-file
               (funcall newfile-template filepath)
               ;; (cond
               ;;  ((functionp newfile-template) (funcall newfile-template filepath))
               ;;  ((stringp newfile-template) (s-lex-format newfile-template)))
               t))))))))

(defun projectile-grid-find-model ()
  "Find a model."
  (interactive)
  (projectile-grid-find-resource
   "model: "
   '(("python/urbint_lib/models/"
      "\\(.+\\)\\.py$")
     ("python/urbint_lib/"
      "\\(.+\\)/models/\\(.+\\).py$"))
   (lambda (filename)
     (pcase (s-split "/" filename)
       (`(,model)
        (s-lex-format "python/urbint_lib/models/${model}.py"))
       (`(,app ,model)
        (s-lex-format "python/urbint_lib/${app}/models/${model}.py"))))))

(defun projectile-grid-find-controller ()
  "Find a controller."
  (interactive)
  (projectile-grid-find-resource
   "controller: "
   '(("backend/src/grid/api/controllers/"
      "\\(.+\\)\\.py$")
     ("backend/src/grid/api/apps/"
      "\\(.+\\)/controllers/\\(.+\\).py$"))
   (lambda (filename)
     (pcase (s-split "/" filename)
       (`(,controller)
        (s-lex-format "backend/src/grid/api/controllers/${controller}.py"))
       (`(,app ,controller)
        (s-lex-format "backend/src/grid/api/apps/${app}/controllers/${controller}.py"))))))

(defvar projectile-grid-mode-map
  (let ((map (make-keymap)))
    (map!
     (:map map
      (:leader
       (:desc "Edit..." :prefix "e"
        :desc "Model"      :n "m" #'projectile-grid-find-model
        :desc "Controller" :n "c" #'projectile-grid-find-controller))))))

(define-minor-mode projectile-grid-mode
  "Minor mode for finding files in GRID"
  :init-value nil
  :lighter " GRID"
  :keymap projectile-grid-mode-map)
