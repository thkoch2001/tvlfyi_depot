(in-package #:cl-user)
(defpackage #:server
  (:documentation "Robot condemned to a life of admin work for my blog.")
  (:use #:cl)
  (:import-from #:cl-arrows #:->>)
  (:export :main))
(in-package #:server)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Nix-injected dependencies
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: Wrap this in an assert or ensure that there's a trailing slash so it's
;; treated as a directory.
(defvar *path-to-posts* "/tmp/"
  "File path pointing to the posts directory.")

(defvar *pandoc-bin* "/usr/bin/pandoc")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Library
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun render-post (path)
  "Render the markdown file stored at PATH to HTML using pandoc."
  (uiop:run-program (list *pandoc-bin* path "--to" "html")
                    :output :string))

;; TODO: Figure out how to handle this with Nix.
(defvar *posts* (uiop:directory-files *path-to-posts*)
  "List of the paths to the blog posts.")

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
