(in-package #:cl-user)
(defpackage #:server
  (:documentation "Robot condemned to a life of admin work for my blog.")
  (:use #:cl)
  (:use #:cl-ppcre)
  (:import-from #:cl-arrows #:->>)
  (:export :main))
(in-package #:server)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Nix-injected dependencies
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *path-to-posts* "/tmp/"
  "File path pointing to the posts directory.")

(defvar *pandoc-bin* "/usr/bin/pandoc")

(defvar *html-template* "./index.html"
  "The path to the HTML template used for the blog posts.")

(defvar *posts* (uiop:directory-files *path-to-posts*)
  "List of the paths to the blog posts.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Library
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: Support properly indenting the output from pandoc to nest within the
;; template. Or just use a proper templating library.
(defun render-post (path)
  "Render the markdown file stored at PATH to HTML using pandoc."
  (cl-ppcre:regex-replace-all
   "{{ blog }}"
   (uiop:read-file-string *html-template*)
   (uiop:run-program (list *pandoc-bin* path "--to" "html") :output :string)))

(hunchentoot:define-easy-handler
    (get-latest :uri "/latest") ()
  (render-post (concatenate 'string *path-to-posts* "/" "test.md")))

(hunchentoot:define-easy-handler
    (get-posts :uri "/posts") ()
  "Working!")

(defun main ()
  "This is the main entrypoint for our application."
  (hunchentoot:start (make-instance 'hunchentoot:easy-acceptor :port 4242))
  (print "Listing on port 4242...")
  (sb-thread:join-thread
   (find-if (lambda (th)
              (string= (sb-thread:thread-name th)
                       "hunchentoot-listener-*:4242"))
            (sb-thread:list-all-threads))))
