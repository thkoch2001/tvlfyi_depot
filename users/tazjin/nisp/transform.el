;; Nix as a Lisp

(require 'cl-lib)
(require 'json)
(require 's)
(require 'dash)

(defun nisp/expr (form)
  "Entrypoint for Nisp->Nix transformation. Will translate FORM
into Nix code, if it is a valid Nisp expression.

To make code generation slightly easier, each
expression (including literals) is wrapped in an extra pair of
parens."
  (concat
   "("
   (pcase form
     ;; Special keywords
     ('() "null")
     (`(let . ,rest) (nisp/let form))
     (`(fn . ,rest) (nisp/fn form))
     (`(if ,cond ,then ,else) (nisp/if cond then else))

     ;; Nix operators & builtins that need special handling
     (`(or  ,lhs ,rhs) (nisp/infix "||" lhs rhs))
     (`(and ,lhs ,rhs) (nisp/infix "&&" lhs rhs))
     (`(> ,lhs ,rhs) (nisp/infix ">" lhs rhs))
     (`(< ,lhs ,rhs) (nisp/infix "<" lhs rhs))
     (`(>= ,lhs ,rhs) (nisp/infix ">=" lhs rhs))
     (`(<= ,lhs ,rhs) (nisp/infix "<=" lhs rhs))
     (`(+ ,lhs ,rhs) (nisp/infix "+" lhs rhs))
     (`(- ,lhs ,rhs) (nisp/infix "-" lhs rhs))
     (`(* ,lhs ,rhs) (nisp/infix "*" lhs rhs))
     (`(/ ,lhs ,rhs) (nisp/infix "/" lhs rhs))
     (`(-> ,lhs ,rhs) (nisp/infix "->" lhs rhs))
     (`(? ,lhs ,rhs) (nisp/infix "?" lhs rhs))
     (`(// ,lhs ,rhs) (nisp/infix "//" lhs rhs))
     (`(++ ,lhs ,rhs) (nisp/infix "++" lhs rhs))
     (`(== ,lhs ,rhs) (nisp/infix "==" lhs rhs))
     (`(!= ,lhs ,rhs) (nisp/infix "!=" lhs rhs))
     (`(! ,term) (concat "!" (nisp/expr term)))
     (`(- ,term) (concat "-" (nisp/expr term)))

     ;; Attribute sets
     (`(attrs . ,rest) (nisp/attribute-set form))

     ;; Function calls
     ((and `(,func . ,args)
           (guard (symbolp func)))
      (nisp/funcall func args))

     ;; Primitives
     ((pred stringp) (json-encode-string form))
     ((pred numberp) (json-encode-number form))
     ((pred keywordp) (substring (symbol-name form) 1))
     ((pred symbolp) (symbol-name form))

     ;; Lists
     ((pred arrayp) (nisp/list form))

     (other (error "Encountered unhandled form: %s" other)))
   ")"))

(defun nisp/infix (op lhs rhs)
  (concat (nisp/expr lhs) " " op " " (nisp/expr rhs)))

(defun nisp/funcall (func args)
  (concat (symbol-name func) " " (s-join " " (-map #'nisp/expr args))))

(defun nisp/let (form)
  (pcase form
    (`(let . (,bindings . (,body . ()))) (concat "let "
                                                 (nisp/let bindings)
                                                 (nisp/expr body)))
    (`((:inherit . ,inherits) . ,rest) (concat (nisp/inherit (car form))
                                               " "
                                               (nisp/let rest)))
    (`((,name . (,value . ())) .,rest) (concat (symbol-name name) " = "
                                               (nisp/expr value) "; "
                                               (nisp/let rest)))
    ('() "in ")
    (other (error "malformed form '%s' in let expression" other))))

(defun nisp/inherit (form)
  (pcase form
    (`(:inherit . ,rest) (concat "inherit " (nisp/inherit rest)))
    (`((,source) . ,rest) (concat "(" (symbol-name source) ") " (nisp/inherit rest)))
    (`(,item . ,rest) (concat (symbol-name item) " " (nisp/inherit rest)))
    ('() ";")))

(defun nisp/if (cond then else)
  (concat "if " (nisp/expr cond)
          " then " (nisp/expr then)
          " else " (nisp/expr else)))

(defun nisp/list (form)
  (cl-check-type form array)
  (concat "[ "
          (mapconcat #'nisp/expr form " ")
          "]"))


(defun nisp/attribute-set (form)
  "Attribute sets have spooky special handling because they are
not supported by the reader."
  (pcase form
    (`(attrs . ,rest) (concat "{ " (nisp/attribute-set rest)))
    ((and `(,name . (,value . ,rest))
          (guard (keywordp name)))
     (concat (substring (symbol-name name) 1) " = "
             (nisp/expr value) "; "
             (nisp/attribute-set rest)))
    ('() "}")))

(defun nisp/fn (form)
  (pcase form
    (`(fn ,args ,body) (concat
                              (cl-loop for arg in args
                                       concat (format "%s: " arg))
                              (nisp/expr body)))))

;; The following functions are not part of the transform.

(defun nisp/eval (form)
  (interactive "sExpression: ")
  (when (stringp form)
    (setq form (read form)))

  (message
   ;; TODO(tazjin): Construct argv manually to avoid quoting issues.
   (s-chomp
    (shell-command-to-string
     (concat "nix-instantiate --eval -E '" (nisp/expr form) "'")))))

(defun nisp/eval-last-sexp ()
  (interactive)
  (nisp/eval (edebug-last-sexp)))
