(defpackage pannetone
  (:use :cl :klatre)
  (:export :start-pannetone :config :main))
(in-package :pannetone)

(declaim (optimize (safety 3)))

;;; Data model

(defclass issue-comment ()
  ((body :reader body
         :initarg :body
         :type string)
   (author-id :type integer
              :initarg :author-id
              :reader author-id)))

(defclass issue (cl-prevalence:object-with-id)
  ((subject :type string
            :initarg :subject
            :reader subject)
   (author-id :type integer
              :initarg :author-id
              :reader author-id)
   (body :type string
         :initarg :body
         :reader body)
   (comments :initarg :comments
             :initform nil
             :type list
             :reader issue-comments)
   (created-at :type integer
               :initform (get-universal-time)
               :reader created-at)))

(defclass user ()
  ((cn :reader cn
       :initarg :cn
       :type string)
   (email :reader email
          :initarg :email
          :type string)))

;;; Persistence

(defvar *p-system* nil
    "The persistence system for this instance of Pannetone")

(define-condition issue-not-found (error)
  ((id :type integer
       :initarg :id
       :reader not-found-id
       :documentation "ID of the issue that was not found"))
  (:documentation
   "Error condition for when an issue requested by ID is not
  found"))

(defun get-issue (system id)
  (restart-case
      (or
       (cl-prevalence:find-object-with-id system 'issue id)
       (error 'issue-not-found :id id))
    (different-id (new-id)
      :report "Use a different issue ID"
      :interactive (lambda ()
                     (format t "Enter a new ID: ")
                     (multiple-value-list (eval (read))))
      (get-issue system new-id))))

(defun list-issues (system)
  (cl-prevalence:find-all-objects system 'issue))

(defun create-issue (system &rest attrs)
  (cl-prevalence:tx-create-object
   system
   'issue
   (chunk-list 2 attrs)))

(defun add-comment (system issue-id &rest attrs)
  "Add a comment with the given ATTRS to the issue ISSUE-ID, and return the
updated issue"
  (let* ((comment (apply #'make-instance 'issue-comment attrs))
         (issue (get-issue system issue-id))
         (comments (append (issue-comments issue)
                           (list comment))))
    (cl-prevalence:tx-change-object-slots
     system
     'issue
     issue-id
     `((comments ,comments)))
    (setf (slot-value issue 'comments) comments)
    comments))

(defun initialize-persistence (data-dir)
  "Initialize the Pannetone persistence system, storing data in DATA-DIR"
  (ensure-directories-exist data-dir)
  (setq *p-system* (cl-prevalence:make-prevalence-system data-dir))

  (unless (posp (length (list-issues *p-system*)))
    (cl-prevalence:tx-create-id-counter *p-system*)))

;;; Web (TODO)

(comment
 (initialize-persistence "/tmp/pannetone")
 (cl-prevalence:execute-transaction
  (create-issue *p-system* 'subject "test"))

 (list-issues *p-system*)

 (add-comment *p-system* 1 :body "hi")


 (let ((issue )))

 (cl-prevalence:snapshot *p-system*)

 (delete-file "/tmp/snapshot.xml ")

 )

;; Issue
;; Number
;; Author (user id)
;; Subject
;; Body
;; Status
;; Open
;; Closed

;; Comment
;; Issue ID
;; Author (user id)
;; Body

;; Issue subscriber
;; Issue id
;; User id
