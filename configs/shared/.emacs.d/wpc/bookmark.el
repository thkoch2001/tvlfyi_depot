;;; bookmark.el --- Saved files and directories on my filesystem -*- lexical-binding: t -*-
;; Author: William Carroll <wpcarro@gmail.com>

;;; Commentary:
;; After enjoying and relying on Emacs's builtin `jump-to-register' command, I'd
;; like to recreate this functionality with a few extensions.
;;
;; Everything herein will mimmick my previous KBDs for `jump-to-register', which
;; were <leader>-j-<register-kbd>.  If the `bookmark-path' is a file, Emacs will
;; open a buffer with that file.  If the `bookmark-path' is a directory, Emacs
;; will open an ivy window searching that directory.

;;; Code:

(require 'f)
(require 'buffer)
(require 'list)
(require 'string)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl-defstruct bookmark label path kbd)

(defconst bookmark/install-kbds? t
  "When t, install keybindings.")

;; TODO: Consider hosting this function somewhere other than here, since it
;; feels useful above of the context of bookmarks.
;; TODO: Assess whether it'd be better to use the existing function:
;; `counsel-projectile-switch-project-action'.  See the noise I made on GH for
;; more context: https://github.com/ericdanan/counsel-projectile/issues/137

(defun bookmark/handle-directory-dwim (path)
  "Open PATH as either a project directory or a regular directory.
If PATH is `projectile-project-p', open with `counsel-projectile-find-file'.
Otherwise, open with `counsel-find-file'."
  (if (projectile-project-p path)
      (with-temp-buffer
        (cd (projectile-project-p path))
        (call-interactively #'counsel-projectile-find-file))
    (let ((ivy-extra-directories nil))
      (counsel-find-file path))))

(defconst bookmark/handle-directory #'bookmark/handle-directory-dwim
  "Function to call when a bookmark points to a directory.")

(defconst bookmark/handle-file #'counsel-find-file-action
  "Function to call when a bookmark points to a file.")

(defconst bookmark/whitelist
  (list
   (make-bookmark :label "tazjins-depot"
                  :path "~/Dropbox/programming/tazjins-depot/"
                  :kbd "t")
   (make-bookmark :label "org"
                  :path "~/Dropbox/org"
                  :kbd "o")
   (make-bookmark :label "mono"
                  :path "~/mono"
                  :kbd "m")
   (make-bookmark :label "dotfiles"
                  :path "~/dotfiles"
                  :kbd "d")
   (make-bookmark :label "current project"
                  :path constants/current-project
                  :kbd "p"))
  "List of registered bookmarks.")


;; TODO: Consider `ivy-read' extension that takes a list of structs,
;; `struct-to-label' and `label-struct' functions.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; API
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun bookmark/open (b)
  "Open bookmark, B, in a new buffer or an ivy minibuffer."
  (let ((path (bookmark-path b)))
    (cond
     ((f-directory? path)
      (funcall bookmark/handle-directory path))
     ((f-file? path)
      (funcall bookmark/handle-file path)))))

(defun bookmark/ivy-open ()
  "Use ivy to filter available bookmarks."
  (interactive)
  (ivy-read "Bookmark: "
            (->> bookmark/whitelist
                 (list/map #'bookmark-label))
            :require-match t
            :action (lambda (label)
                      (->> bookmark/whitelist
                           (list/find
                            (lambda (b)
                              (equal label (bookmark-label b))))
                           bookmark/open))))

(when bookmark/install-kbds?
  (general-define-key
   :prefix "<SPC>"
   :states '(normal)
   "jj" #'bookmark/ivy-open)
  (->> bookmark/whitelist
       (list/map
        (lambda (b)
          (general-define-key
           :prefix "<SPC>"
           :states '(normal)
           (string/concat "j" (bookmark-kbd b))
           ;; TODO: Consider `cl-labels' so `which-key' minibuffer is more
           ;; helpful.
           (lambda () (interactive) (bookmark/open b)))))))

(provide 'bookmark)
;;; bookmark.el ends here
