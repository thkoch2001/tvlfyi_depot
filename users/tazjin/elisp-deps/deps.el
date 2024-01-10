;; Visualise the internal structure of an Emacs Lisp file using
;; Graphviz.
;;
;; Entry point is the function `edeps-analyse-file'.

(require 'map)

(defun edeps-read-defs (file-name)
  "Stupidly read all definitions from an Emacs Lisp file. This only
considers top-level forms, where the first element of the form is
a symbol whose name contains the string `def', and where the
second element is a symbol.

Returns a hashmap of all these symbols with the remaining forms
in their bodies."

  (with-temp-buffer
    (insert-file-contents file-name)
    (goto-char (point-min))

    (let ((symbols (make-hash-table)))
      (condition-case _err
          (while t
            (let ((form (read (current-buffer))))
              (when (and (listp form)
                         (symbolp (car form))
                         (string-match "def" (symbol-name (car form)))
                         (symbolp (cadr form)))
                (when (and (map-contains-key symbols (cadr form))
                           ;; generic methods have multiple definitions
                           (not (eq (car form) 'cl-defmethod)))
                  (error "Duplicate symbol: %s" (symbol-name (cadr form))))

                (map-put! symbols (cadr form)
                          (cons (car form) (cddr form))))))
        (end-of-file symbols)))))

(defun edeps-analyse-structure (symbols)
  "Analyse the internal structure of the symbols found by
edeps-read-defs, and return a hashmap with the results of the
analysis. The hashmap uses the symbols as keys, "
  (let ((deps (make-hash-table)))
    (map-do
     (lambda (sym val)
       (dolist (expr (flatten-list (cdr val)))
         (when (map-contains-key symbols expr)
           (map-put! deps expr (cons sym (ht-get deps expr))))))
     symbols)
    deps))

(defun edeps-graph-deps (symbols deps)
  (with-temp-buffer
    (insert "digraph edeps {\n")

    ;; List all symbols first
    (insert "  subgraph {\n")
    (map-do
     (lambda (sym val)
       (insert "    " (format "\"%s\" [label=\"%s\\n(%s)\"];\n" sym sym (car val))))
     symbols)
    (insert "  }\n\n")

    ;; Then drop all the edges in there ..
    (insert "  subgraph {\n")
    (map-do
     (lambda (sym deps)
       (dolist (dep deps)
         (insert "    " (format "\"%s\" -> \"%s\";\n" dep sym))))
     deps)
    (insert "  }\n")

    (insert "}\n")
    (buffer-string)))

(defun edeps-analyse-file (infile outfile)
  "Produces a dot-graph in OUTFILE from an internal structural
analysis of INFILE. This can be graphed using the graphviz
package."
  (let* ((symbols (edeps-read-defs infile))
         (deps (edeps-analyse-structure symbols)))
    (with-temp-buffer
      (insert (edeps-graph-deps symbols deps))
      (write-file outfile))))
