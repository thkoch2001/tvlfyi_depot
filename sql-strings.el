;;; ~/.doom.d/sql-strings.el -*- lexical-binding: t; -*-

;;; https://www.emacswiki.org/emacs/StringAtPoint
(defun ourcomments-string-or-comment-bounds-1 (what)
  (save-restriction
    (widen)
    (let* ((here (point))
           ;; Fix-me: when on end-point, how to handle that and which should be last hit point?
           (state (parse-partial-sexp (point-min) (1+ here)))
           (type (if (nth 3 state)
                     'string
                   (if (nth 4 state)
                       'comment)))
           (start (when type (nth 8 state)))
           end)
      (unless start
        (setq state (parse-partial-sexp (point-min) here))
        (setq type (if (nth 3 state)
                       'string
                     (if (nth 4 state)
                         'comment)))
        (setq start (when type (nth 8 state))))
      (unless (or (not what)
                  (eq what type))
        (setq start nil))
      (if (not start)
          (progn
            (goto-char here)
            nil)
        (setq state (parse-partial-sexp (1+ start) (point-max)
                                        nil nil state 'syntax-table))
        (setq end (point))
        (goto-char here)
        (cons start end)))))

(defun ourcomments-bounds-of-string-at-point ()
  "Return bounds of string at point if any."
  (ourcomments-string-or-comment-bounds-1 'string))

(put 'string 'bounds-of-thing-at-point 'ourcomments-bounds-of-string-at-point)

(defun -sanitize-sql-string (str)
  (->> str
       (downcase)
       (s-trim)
       (replace-regexp-in-string
        (rx (or (and string-start (or "\"\"\""
                                      "\""))
                (and (or "\"\"\""
                         "\"")
                     string-end)))
        "")
       (s-trim)))

(defun sql-string-p (str)
  "Returns 't if STR looks like a string literal for a SQL statement"
  (setq str (-sanitize-sql-string str))
  (or (s-starts-with? "select" str)))

;;; tests

(require 'ert)

(ert-deftest sanitize-sql-string-test ()
  (should (string-equal "select * from foo;"
                        (-sanitize-sql-string
                         "\"\"\"SELECT * FROM foo;\n\n\"\"\""))))

(ert-deftest test-sql-string-p ()
  (dolist (str '("SELECT * FROM foo;"
                 "select * from foo;"))
    (should (sql-string-p str)))

  (dolist (str '("not a QUERY"))
    (should-not (sql-string-p str))))
