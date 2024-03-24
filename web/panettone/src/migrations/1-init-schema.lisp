"Initialize the database schema from before migrations were added"

(defun ddl/create-issue-status ()
  "Issue DDL to create the `issue-status' type, if it doesn't exist"
  (unless (query (:select (:exists (:select 1
                                    :from 'pg_type
                                    :where (:= 'typname "issue_status"))))
                 :single)
    (query (sql-compile
            `(:create-enum issue-status ,panettone.model:+issue-statuses+)))))

(defun ddl/create-tables ()
  "Issue DDL to create all tables, if they don't already exist."
  (dolist (table '(panettone.model:issue
                   panettone.model:issue-comment
                   panettone.model:issue-event
                   panettone.model:user-settings))
    (unless (table-exists-p (dao-table-name table))
      (create-table table))))

(defun up ()
  (ddl/create-issue-status)
  (ddl/create-tables))
