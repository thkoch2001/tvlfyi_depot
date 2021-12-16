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

(defun make-thread
    (function &rest args)
  "Make a new thread as per `BORDEAUX-THREADS:MAKE-THREAD' but with its own, new
database connection."
  (let ((spec `(,(or (uiop:getenvp "PGDATABASE") "panettone")
                ,(or (uiop:getenvp "PGUSER") "panettone")
                ,(or (uiop:getenvp "PGPASSWORD") "password")
                ,(or (uiop:getenvp "PGHOST") "localhost")
                :port ,(or (integer-env "PGPORT") 5432))))
    (apply #'bt:make-thread
           (lambda ()
             (postmodern:call-with-connection spec function))
           args)))

;;;
;;; Schema
;;;

(defclass user-settings ()
  ((user-dn :col-type string :initarg :user-dn :accessor user-dn)
   (enable-email-notifications
    :col-type boolean
    :initarg :enable-email-notifications
    :accessor enable-email-notifications-p
    :initform t
    :col-default t))
  (:metaclass dao-class)
  (:keys user-dn)
  (:table-name user_settings)
  (:documentation
   "Panettone settings for an individual user DN"))

(deftable (user-settings "user_settings")
  (!dao-def))

(defun settings-for-user (dn)
  "Retrieve the settings for the user with the given DN, creating a new row in
  the database if not yet present"
  (or
   (car
    (query-dao
     'user-settings
     (:select '* :from 'user-settings :where (:= 'user-dn dn))))
   (insert-dao (make-instance 'user-settings :user-dn dn))))

(defun update-user-settings (settings &rest attrs)
  "Update the fields of the settings for USER with the given ATTRS, which is a
  plist of slot and value"
  (check-type settings user-settings)
  (when-let ((set-fields
              (iter
                (for slot in '(enable-email-notifications))
                (for new-value = (getf attrs slot))
                (appending
                 (progn
                   (setf (slot-value settings slot) new-value)
                   (list slot new-value))))))
    (execute
     (sql-compile
      `(:update user-settings
        :set ,@set-fields
        :where (:= user-dn ,(user-dn settings)))))))


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

(defclass has-created-at ()
  ((created-at :col-type timestamp
               :col-default (local-time:now)
               :initarg :created-at
               :accessor created-at))
  (:metaclass dao-class))

(defun created-at->timestamp (object)
  (assert (slot-exists-p object 'created-at))
  (unless (or (not (slot-boundp object 'created-at))
              (typep (slot-value object 'created-at) 'local-time:timestamp))
    (setf (slot-value object 'created-at)
          (local-time:universal-to-timestamp (created-at object)))))

(defmethod initialize-instance :after
    ((obj has-created-at) &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (created-at->timestamp obj))

(defun keyword->str (kw) (string-downcase (symbol-name kw)))
(defun str->keyword (st) (alexandria:make-keyword (string-upcase st)))

(defclass issue (has-created-at)
  ((id :col-type serial :initarg :id :accessor id)
   (subject :col-type string :initarg :subject :accessor subject)
   (body :col-type string :initarg :body :accessor body :col-default "")
   (author-dn :col-type string :initarg :author-dn :accessor author-dn)
   (comments :type list :accessor issue-comments)
   (events :type list :accessor issue-events)
   (num-comments :type integer :accessor num-comments)
   (status :col-type issue_status
           :initarg :status
           :accessor status
           :initform :open
           :col-default "open"
           :col-export keyword->str
           :col-import str->keyword))
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
(defmethod cl-postgres:to-sql-string ((ts local-time:timestamp))
  (cl-postgres:to-sql-string
   (local-time:timestamp-to-unix ts)))

(defmethod initialize-instance :after
    ((issue issue) &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (unless (symbolp (status issue))
    (setf (status issue)
          (intern (string-upcase (status issue))
                  "KEYWORD"))))

(deftable issue (!dao-def))

(defclass issue-comment (has-created-at)
  ((id :col-type integer :col-identity t :initarg :id :accessor id)
   (body :col-type string :initarg :body :accessor body)
   (author-dn :col-type string :initarg :author-dn :accessor author-dn)
   (issue-id :col-type integer :initarg :issue-id :accessor :user-id))
  (:metaclass dao-class)
  (:keys id)
  (:table-name issue_comments)
  (:documentation "Comments on an `issue'"))
(deftable (issue-comment "issue_comments")
  (!dao-def)
  (!foreign 'issues 'issue-id 'id :on-delete :cascade :on-update :cascade))

(defclass issue-event (has-created-at)
  ((id :col-type integer :col-identity t :initarg :id :accessor id)
   (issue-id :col-type integer
             :initarg :issue-id
             :accessor issue-id)
   (acting-user-dn :col-type string
                   :initarg :acting-user-dn
                   :accessor acting-user-dn)
   (field :col-type (or string db-null)
          :initarg :field
          :accessor field)
   (previous-value :col-type (or string db-null)
                   :initarg :previous-value
                   :accessor previous-value)
   (new-value :col-type (or string db-null)
              :initarg :new-value
              :accessor new-value))
  (:metaclass dao-class)
  (:keys id)
  (:table-name issue_events)
  (:documentation "Events that have occurred for an issue.

If a field has been changed on an issue, the SYMBOL-NAME of that slot will be in
FIELD, its previous value will be formatted using ~A into PREVIOUS-VALUE, and
its new value will be formatted using ~A into NEW-VALUE"))

(deftable (issue-event "issue_events")
  (!dao-def)
  (!foreign 'issues 'issue-id 'id :on-delete :cascade :on-update :cascade))

(define-constant +all-tables+
    '(issue
      issue-comment
      issue-event
      user-settings)
  :test #'equal)

(defun ddl/create-tables ()
  "Issue DDL to create all tables, if they don't already exist."
  (dolist (table +all-tables+)
    (unless (table-exists-p (dao-table-name table))
      (create-table table))))

(defun ddl/init ()
  "Idempotently initialize the full database schema for Panettone"
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

(defmethod slot-unbound (cls (issue issue) (slot (eql 'events)))
  (declare (ignore cls) (ignore slot))
  (setf (issue-events issue) (issue-events (id issue))))

(defmethod issue-events ((issue-id integer))
  "Return a list of all events with the given ISSUE-ID, sorted oldest first.
NOTE: This makes a database query, so be wary of N+1 queries"
  (query-dao
   'issue-event
   (:order-by
    (:select '*
     :from 'issue-events
     :where (:= 'issue-id issue-id))
    (:asc 'created-at))))


;;;
;;; Writing
;;;

(defun record-issue-event
    (issue-id &key
                field
                previous-value
                new-value)
  "Record in the database that the user identified by `AUTHN:*USER*' updated
ISSUE-ID, and return the resulting `ISSUE-EVENT'. If no user is currently
authenticated, warn and no-op"
  (check-type issue-id (integer))
  (check-type field (or null symbol))
  (if authn:*user*
      (insert-dao
       (make-instance 'issue-event
                      :issue-id issue-id
                      :acting-user-dn (authn:dn authn:*user*)
                      :field (symbol-name field)
                      :previous-value (when previous-value
                                        (format nil "~A" previous-value))
                      :new-value (when new-value
                                   (format nil "~A" new-value))))
      (warn "Performing operation as unauthenticated user")))

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
  (let ((original-status (query (:select 'status
                                 :from 'issues
                                 :where (:= 'id issue-id))
                                :single)))
    (when (zerop (execute (:update 'issues
                           :set 'status (cl-postgres:to-sql-string status)
                           :where (:= 'id issue-id))))
      (error 'issue-not-found :id issue-id))
    (record-issue-event
     issue-id
     :field 'status
     :previous-value (string-upcase original-status)
     :new-value status)
    (values)))

(defun update-issue (issue &rest attrs)
  "Update the fields of ISSUE with the given ATTRS, which is a plist of slot and
value, and record events for the updates"
  (let ((set-fields
          (iter (for slot in '(subject body))
            (for new-value = (getf attrs slot))
            (appending
             (let ((previous-value (slot-value issue slot)))
               (when (and new-value (not (equalp
                                          new-value
                                          previous-value)))
                 (record-issue-event (id issue)
                                     :field slot
                                     :previous-value previous-value
                                     :new-value new-value)
                 (setf (slot-value issue slot) new-value)
                 (list slot new-value)))))))
    (execute
     (sql-compile
      `(:update issues
        :set ,@set-fields
        :where (:= id ,(id issue)))))))

(defun create-issue-comment (&rest attrs &key issue-id &allow-other-keys)
  "Insert a new issue comment into the database with the given ATTRS and
ISSUE-ID, which should be a plist of initforms, and return an instance of
`issue-comment'. If no issue exists with `ID' ISSUE-ID, signals
`issue-not-found'."
  (unless (issue-exists-p issue-id)
    (error 'issue-not-found :id issue-id))
  (insert-dao (apply #'make-instance 'issue-comment :issue-id issue-id attrs)))

(defun issue-commenter-dns (issue-id)
  "Returns a list of all the dns of users who have commented on ISSUE-ID"
  (query (:select 'author-dn :distinct
          :from 'issue-comments
          :where (:= 'issue-id issue-id))
         :column))

(defun issue-subscribers (issue-id)
  "Returns a list of user DNs who should receive notifications for actions taken
  on ISSUE-ID.

Currently this is implemented as the author of issue plus all the users who have
commented on the issue, but in the future we likely want to also allow
explicitly subscribing to / unsubscribing from individual issues."
  (let ((issue (get-issue issue-id)))
    (adjoin (author-dn issue)
            (issue-commenter-dns issue-id)
            :test #'equal)))


(comment
 (connect-postgres)
 (ddl/init)
 (make-instance 'issue :subject "test")
 (create-issue :subject "test"
               :author-dn "cn=grfn,ou=users,dc=tvl,dc=fyi")

 (issue-commenter-dns 1)
 (issue-subscribers 1)

 )
