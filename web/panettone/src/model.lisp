(in-package :panettone.model)
(declaim (optimize (safety 3)))

(defun connect-postgres (&key
                           (host (or (uiop:getenvp "PGHOST") "localhost"))
                           (user (or (uiop:getenvp "PGUSER") "panettone"))
                           (password (or (uiop:getenvp "PGPASSWORD") "password"))
                           (database (or (uiop:getenvp "PGDATABASE") "panettone"))
                           (port (or (integer-env "PGPORT") 5432)))
  "Initialize the global postgresql connection for Panettone"
  (postmodern:connect-toplevel database user password host :port port))

;;;
;;; Schema
;;;

(define-constant +issue-statuses+ '(:open :closed)
  :test #'equal)

(deftype issue-status ()
  "Type specifier for the status of an `issue'"
  (cons 'member +issue-statuses+))

(defun ddl/create-issue-status ()
  "Issue DDL to create the `issue-status' type, if it doesn't exist"
  (unless (query (:select (:exists (:select 1
                                    :from 'pg_type
                                    :where (:= 'typname "issue_status"))))
                 :single)
    (query (sql-compile
            `(:create-enum issue-status ,+issue-statuses+)))))

(defclass issue ()
  ((id :col-type serial :initarg :id :accessor id)
   (subject :col-type string :initarg :subject :accessor subject)
   (body :col-type string :initarg :body :accessor body :col-default "")
   (author-dn :col-type string :initarg :author-dn :accessor author-dn)
   (comments :type list :accessor issue-comments)
   (num-comments :type integer :accessor num-comments)
   (status :col-type issue_status
           :initarg :status
           :accessor status
           :initform :open
           :col-default "open")
   (created-at :col-type timestamp
               :col-default (local-time:now)
               :accessor created-at))
  (:metaclass dao-class)
  (:keys id)
  (:table-name issues)
  (:documentation
   "Issues are the primary entity in the Panettone database. An issue is
   reported by a user, has a subject and an optional body, and can be either
   open or closed"))

(defmethod cl-postgres:to-sql-string ((kw (eql :open)))
  (cl-postgres:to-sql-string "open"))
(defmethod cl-postgres:to-sql-string ((kw (eql :closed)))
  (cl-postgres:to-sql-string "closed"))

(defun created-at->timestamp (object)
  (assert (slot-exists-p object 'created-at))
  (unless (or (not (slot-boundp object 'created-at))
              (typep (slot-value object 'created-at) 'local-time:timestamp))
    (setf (slot-value object 'created-at)
          (local-time:universal-to-timestamp (created-at object)))))

(defmethod initialize-instance :after
    ((issue issue) &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (unless (symbolp (status issue))
    (setf (status issue)
          (intern (string-upcase (status issue))
                  "KEYWORD")))
  (created-at->timestamp issue))

(deftable issue (!dao-def))

(defclass issue-comment ()
  ((id :col-type integer :col-identity t :initarg :id :accessor id)
   (body :col-type string :initarg :body :accessor body)
   (author-dn :col-type string :initarg :author-dn :accessor author-dn)
   (issue-id :col-type integer :initarg :issue-id :accessor :user-id)
   (created-at :col-type timestamp
               :col-default (local-time:now)
               :accessor created-at))
  (:metaclass dao-class)
  (:keys id)
  (:table-name issue_comments)
  (:documentation "Comments on an `issue'"))
(deftable (issue-comment "issue_comments")
  (!dao-def)
  (!foreign 'issues 'issue-id 'id :on-delete :cascade :on-update :cascade))

(defmethod initialize-instance :after
    ((issue-comment issue-comment) &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (created-at->timestamp issue-comment))

(defun ddl/create-tables ()
  "Issue DDL to create all tables, if they don't already exist."
  (dolist (table '(issue issue-comment))
    (unless (table-exists-p (dao-table-name table))
      (create-table table))))

(defun ddl/init ()
  "Idempotently nitialize the full database schema for Panettone"
  (ddl/create-issue-status)
  (ddl/create-tables))

;;;
;;; Querying
;;;

(define-condition issue-not-found (error)
  ((id :type integer
       :initarg :id
       :reader not-found-id
       :documentation "ID of the issue that was not found"))
  (:documentation
   "Error condition for when an issue requested by ID is not found"))

(defun get-issue (id)
  "Look up the 'issue with the given ID and return it, or signal a condition of
type `ISSUE-NOT-FOUND'."
  (restart-case
      (or (get-dao 'issue id)
          (error 'issue-not-found :id id))
    (different-id (new-id)
      :report "Use a different issue ID"
      :interactive (lambda ()
                     (format t "Enter a new ID: ")
                     (multiple-value-list (eval (read))))
      (get-issue new-id))))

(defun issue-exists-p (id)
  "Returns `T' if an issue with the given ID exists"
  (query
   (:select (:exists (:select 1
                      :from 'issues
                      :where (:= 'id id))))
   :single))

(defun list-issues (&key status (with '(:num-comments)))
  "Return a list of all issues with the given STATUS (or all if nil), ordered by
  ID descending. If WITH contains `:NUM-COMMENTS' (the default) each issue will
  have the `num-comments' slot filled with the number of comments on that issue
  (to avoid N+1 queries)."
  (let* ((condition (unless (null status)
                      `(:where (:= status $1))))
         (select (if (find :num-comments with)
                     `(:select issues.* (:as (:count issue-comments.id)
                                             num-comments)
                               :from issues
                               :left-join issue-comments
                               :on (:= issues.id issue-comments.issue-id)
                               ,@condition
                               :group-by issues.id)
                     `(:select * :from issues ,@condition)))
         (query (sql-compile
                 `(:order-by ,select (:desc id)))))
    (with-column-writers ('num_comments 'num-comments)
      (query-dao 'issue query status))))

(defmethod num-comments ((issue-id integer))
  "Return the number of comments for the given ISSUE-ID."
  (query
   (:select (:count '*)
    :from 'issue-comments
    :where (:= 'issue-id issue-id))
   :single))

(defmethod slot-unbound (cls (issue issue) (slot (eql 'comments)))
  (declare (ignore cls) (ignore slot))
  (setf (issue-comments issue) (issue-comments (id issue))))

(defmethod issue-comments ((issue-id integer))
  "Return a list of all comments with the given ISSUE-ID, sorted oldest first.
NOTE: This makes a database query, so be wary of N+1 queries"
  (query-dao
   'issue-comment
   (:order-by
    (:select '*
     :from 'issue-comments
     :where (:= 'issue-id issue-id))
    (:asc 'created-at))))

;;;
;;; Writing
;;;

(defun create-issue (&rest attrs)
  "Insert a new issue into the database with the given ATTRS, which should be
a plist of initforms, and return an instance of `issue'"
  (insert-dao (apply #'make-instance 'issue attrs)))

(defun delete-issue (issue)
  (delete-dao issue))

(defun set-issue-status (issue-id status)
  "Set the status of the issue with the given ISSUE-ID to STATUS in the db. If
the issue doesn't exist, signals `issue-not-found'"
  (check-type issue-id integer)
  (check-type status issue-status)
  (when (zerop (execute (:update 'issues
                         :set 'status (cl-postgres:to-sql-string status)
                         :where (:= 'id issue-id))))
    (error 'issue-not-found :id issue-id)))

(defun create-issue-comment (&rest attrs &key issue-id &allow-other-keys)
  "Insert a new issue comment into the database with the given ATTRS and
ISSUE-ID, which should be a plist of initforms, and return an instance of
`issue-comment'. If no issue exists with `ID' ISSUE-ID, signals
`issue-not-found'."
  (unless (issue-exists-p issue-id)
    (error 'issue-not-found :id issue-id))
  (insert-dao (apply #'make-instance 'issue-comment :issue-id issue-id attrs)))

(comment
 (connect-postgres)
 (ddl/init)
 (make-instance 'issue :subject "test")
 (create-issue :subject "test"
               :author-dn "cn=glittershark,ou=users,dc=tvl,dc=fyi")
 )
