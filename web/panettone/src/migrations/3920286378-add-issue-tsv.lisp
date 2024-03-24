"Add tsvector for full-text search of issues"

(defun up ()
  (query "ALTER TABLE issues ADD COLUMN tsv tsvector GENERATED ALWAYS AS (to_tsvector('english', subject || ' ' || body)) STORED")
  (query "CREATE INDEX issues_tsv_index ON issues USING GIN (tsv);"))
