(in-package :panettone.model)
(declaim (optimize (safety 3)))

(setq pomo:*ignore-unknown-columns* t)

(defvar *pg-spec* nil
  "Connection spec for use with the with-connection macro. Needs to be
initialised at launch time.")

(defun make-pg-spec ()
  "Construct the Postgres connection spec from the environment."
  (list (or (uiop:getenvp "PGDATABASE") "panettone")
        (or (uiop:getenvp "PGUSER") "panettone")
        (or (uiop:getenvp "PGPASSWORD") "password")
        (or (uiop:getenvp "PGHOST") "localhost")

        :port (or (integer-env "PGPORT") 5432)
        :application-name "panettone"
        :pooled-p t))

(defun prepare-db-connections ()
  "Initialises the connection spec used for all Postgres connections."
  (setq *pg-spec* (make-pg-spec)))

(defun connect-to-db ()
  "Connect using *PG-SPEC* at the top-level, for use during development"
  (apply #'connect-toplevel
         (loop for v in *pg-spec*
               until (eq v :pooled-p)
               collect v)))

(defun pg-spec->url (&optional (spec *pg-spec*))
  (destructuring-bind (db user password host &key port &allow-other-keys) spec
    (format nil
            "postgres://~A:~A@~A:~A/~A"
            user password host port db)))

;;;
;;; Schema
;;;

(defclass user ()
  ((sub :col-type uuid :initarg :sub :accessor sub
        :documentation
        "ID for the user in the authentication provider. Taken from the `:SUB'
        field in the JWT when the user first logged in")
   (username :col-type string :initarg :username :accessor username)
   (email :col-type string :initarg :email :accessor email))
  (:metaclass dao-class)
  (:keys sub)
  (:table-name users)
  (:documentation
   "Panettone users. Uses an external authentication provider."))

(deftable (user "users")
  (!dao-def))

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

(defclass migration ()
  ((version
    :col-type bigint
    :primary-key t
    :initarg :version
    :accessor version)
   (name :col-type string :initarg :name :accessor name)
   (docstring :col-type string :initarg :docstring :accessor docstring)
   (path :col-type string
         :type pathname
         :initarg :path
         :accessor path
         :col-export namestring
         :col-import parse-namestring)
   (package :type keyword :initarg :package :accessor migration-package))
  (:metaclass dao-class)
  (:keys version)
  (:table-name migrations)
  (:documentation "Migration scripts that have been run on the database"))
(deftable migration (!dao-def))

;;;
;;; Utils
;;;

(defun create-table-if-not-exists (name)
  " Takes the name of a dao-class and creates the table identified by symbol by
executing all forms in its definition as found in the *tables* list, if it does
not already exist."
  (unless (table-exists-p (dao-table-name name))
    (create-table name)))

;;;
;;; Migrations
;;;

(defun ensure-migrations-table ()
  "Ensure the migrations table exists"
  (unless (table-exists-p (dao-table-name 'migration))
    (create-table 'migration)))

(define-build-time-var *migrations-dir* "migrations/"
    "The directory where migrations are stored")

(defun load-migration-docstring (migration-path)
  "If the first form in the file pointed to by `migration-pathname` is
  a string, return it, otherwise return NIL."

  (handler-case
      (with-open-file (s migration-path)
        (when-let ((form (read s)))
          (when (stringp form) form)))
    (t () nil)))

(defun load-migration (path)
  (let* ((parts (str:split #\- (pathname-name path) :limit 2))
         (version (parse-integer (car parts)))
         (name (cadr parts))
         (docstring (load-migration-docstring path))
         (package (intern (format nil "MIGRATION-~A" version)
                          :keyword))
         (migration (make-instance 'migration
                                   :version version
                                   :name name
                                   :docstring docstring
                                   :path path
                                   :package package)))
    (uiop/package:ensure-package package
                                 :use '(#:common-lisp
                                        #:postmodern
                                        #:panettone.model))
    (let ((*package* (find-package package)))
      (load path))

    migration))

(defun run-migration (migration)
  (declare (type migration migration))
  (with-transaction ()
    (format t "Running migration ~A (version ~A)"
            (name migration)
            (version migration))
    (query
     (sql-compile
      `(:delete-from migrations
        :where (= version ,(version migration)))))
    (uiop:symbol-call (migration-package migration) :up)
    (insert-dao migration)))

(defun list-migration-files ()
  (remove-if-not
   (lambda (pn) (string= "lisp" (pathname-type pn)))
   (uiop:directory-files (util:->dir *migrations-dir*))))

(defun load-migrations ()
  (mapcar #'load-migration (list-migration-files)))

(defun generate-migration (name &key documentation)
  "Generate a new database migration with the given NAME, optionally
prepopulated with the given DOCUMENTATION.

Returns the file that the migration is located at, as a `pathname'. Write Lisp
code in this migration file to define a function called `up', which will be run
in the context of a database transaction and should perform the migration."
  (let* ((version (get-universal-time))
         (filename (format nil "~A-~A.lisp"
                           version
                           name))
         (pathname
           (merge-pathnames filename *migrations-dir*)))
    (with-open-file (stream pathname
                            :direction :output
                            :if-does-not-exist :create)
      (when documentation
        (format stream "~S~%~%" documentation))

      (format stream "(defun up ()~%)"))
    pathname))

(defun migrations-already-run ()
  "Query the database for a list of migrations that have already been run"
  (query-dao 'migration (sql-compile '(:select * :from migrations))))

(define-condition migration-name-mismatch ()
  ((version :type integer :initarg :version)
   (name-in-database :type string :initarg :name-in-database)
   (name-in-code :type string :initarg :name-in-code))
  (:report
   (lambda (cond stream)
     (format stream "Migration mismatch: Migration version ~A has name ~S in the database, but we have name ~S"
             (slot-value cond 'version)
             (slot-value cond 'name-in-database)
             (slot-value cond 'name-in-code)))))

(defun migrate ()
  "Migrate the database, running all migrations that have not yet been run"
  (ensure-migrations-table)
  (format t "Running migrations from ~A...~%" *migrations-dir*)
  (let* ((all-migrations (load-migrations))
         (already-run (migrations-already-run))
         (num-migrations-run 0))
    (iter (for migration in all-migrations)
      (if-let ((existing (find-if (lambda (existing)
                                    (= (version existing)
                                       (version migration)))
                                  already-run)))
        (progn
          (unless (string= (name migration)
                           (name existing))
            (restart-case
                (error 'migration-name-mismatch
                       :version (version existing)
                       :name-in-database (name existing)
                       :name-in-code (name migration))
              (skip ()
                :report "Skip this migration"
                (next-iteration))
              (run-and-overwrite ()
                :report "Run this migration anyway, overwriting the previous migration"
                (run-migration migration))))
          (next-iteration))
        ;; otherwise, run the migration
        (run-migration migration))
      (incf num-migrations-run))
    (format t "Ran ~A migration~:P~%" num-migrations-run)))

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

(defun list-issues (&key status search (with '(:num-comments)))
  "Return a list of all issues with the given STATUS (or all if nil), ordered by
  ID descending. If WITH contains `:NUM-COMMENTS' (the default) each issue will
  have the `num-comments' slot filled with the number of comments on that issue
  (to avoid N+1 queries)."
  (let* ((conditions
           (and-where*
            (unless (null status)
              `(:= status $1))
            (when (str:non-blank-string-p search)
              `(:@@ tsv (:websearch-to-tsquery ,search)))))
         (select (if (find :num-comments with)
                     `(:select issues.* (:as (:count issue-comments.id)
                                             num-comments)
                       :from issues
                       :left-join issue-comments
                       :on (:= issues.id issue-comments.issue-id)
                       :where ,conditions
                       :group-by issues.id)
                     `(:select * :from issues :where ,conditions)))
         (order (if (str:non-blank-string-p search)
                    `(:desc (:ts-rank-cd tsv (:websearch-to-tsquery ,search)))
                    `(:desc id)))
         (query (sql-compile
                 `(:order-by ,select ,order))))
    (with-column-writers ('num_comments 'num-comments)
      (query-dao 'issue query status))))

(defmethod count-comments ((issue-id integer))
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

 (make-instance 'issue :subject "test")

 (with-connection *pg-spec*
   (create-issue :subject "test"
                 :author-dn "cn=aspen,ou=users,dc=tvl,dc=fyi"))

 (issue-commenter-dns 1)
 (issue-subscribers 1)

 ;; Creating new migrations
 (setq *migrations-dir* (merge-pathnames "migrations/"))
 (generate-migration "create-users-table"
                     :documentation "Add a table to store information about users")
 (load-migrations)

 ;; Running migrations
 (with-connection *pg-spec*
   (migrate))
 )
