;;;  define.lisp --- grammar rules definition

;;;  Copyright (C) 2003-2006, 2009 by Walter C. Pelissero

;;;  Author: Walter C. Pelissero <walter@pelissero.de>
;;;  Project: NPG a Naive Parser Generator

#+cmu (ext:file-comment "$Module: define.lisp $")

;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Lesser General Public License
;;; as published by the Free Software Foundation; either version 2.1
;;; of the License, or (at your option) any later version.
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Lesser General Public License for more details.
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this library; if not, write to the Free
;;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
;;; 02111-1307 USA

(in-package :naive-parser-generator)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *smart-default-reduction* t
  "If true the default reductions take only the non-static tokens -
those that are not declared as strings in the grammar.")

;; These two are filled with DEFRULE.
(defvar *rules* (make-rules-table))
(defvar *keywords* (make-keywords-table))

(defun make-action-arguments (tokens)
  "Given a list of tokens making up a production, return three values:
the list of variables for the function reducing this production, those
that are non static and their unambiguous user-friendly names."
  (flet ((unique (sym list)
	   (if (not (assoc sym list))
	       sym
	       (loop
		  for i of-type fixnum from 2
		  for x = (intern (format nil "~:@(~A~)~A" sym i))
		  while (assoc x list)
		  finally (return x)))))
    (loop
       for tok in tokens
       for i of-type fixnum from 1
       for arg = (intern (format nil "$~A" i) (find-package #.*package*))
       collect arg into args
       unless (const-terminal-p tok)
         collect arg into vars
         and when (symbolp tok)
           collect (list (unique tok named-vars) arg) into named-vars
       when (and (listp tok)
		 (symbolp (cadr tok)))
	 collect (list (unique (cadr tok) named-vars) arg) into named-vars
       finally
       (return (values args vars named-vars)))))

(defun make-action-function (name tokens action)
  "Create a function with name NAME, arguments derived from TOKENS and
body ACTION.  Return it's definition."
  (let ((function
	 (multiple-value-bind (args vars named-vars)
	     (make-action-arguments tokens)
	   `(lambda ,args
	      (declare (ignorable ,@args))
	      (let (($vars (list ,@vars))
		    ($all (list ,@args))
		    ,@named-vars
		    ($alist (list ,@(mapcar #'(lambda (v)
						`(cons ',(intern (symbol-name (car v)))
						       ,(cadr v)))
					    named-vars))))
		(declare (ignorable $vars $all $alist ,@(mapcar #'car named-vars)))
		(flet ((make-object (&optional type args)
			 (apply #'make-instance (or type ',name)
				(append args $alist))))
		  ,action))))))
    (when *compile-print*
      (if *compile-verbose*
	  (format t "; Compiling ~S:~%  ~S~%" name function)
	  (format t "; Compiling ~S~%" name)))
    (compile name function)))

(defun define-rule (name productions)
  "Accept a rule in EBNF-like syntax, translate it into a sexp and a
call to INSERT-RULE-IN-CURRENT-GRAMMAR."
  (flet ((transform (productions)
	   (loop
	      for tok in productions
	      with prod = nil
	      with action = nil
	      with phase = nil
	      with new-prods = nil
	      while tok
	      do (cond ((eq tok :=)
			(push (list (nreverse prod) action) new-prods)
			(setf prod nil
			      action nil
			      phase :prod))
		       ((eq tok :reduce)
			(setf phase :action))
		       ((eq tok :tag)
			(setf phase :tag))
		       ((eq phase :tag)
			(setf action `(cons ,tok $vars)))
		       ((eq phase :action)
			(setf action tok))
		       ((eq phase :prod)
			(push tok prod)))
	      finally
		(return (cdr (nreverse (cons (list (nreverse prod) action) new-prods)))))))
    (insert-rule-in-current-grammar name (transform productions))))

(defmacro defrule (name &rest productions)
  "Wrapper macro for DEFINE-RULE."
  `(define-rule ',name ',productions))

(defun make-optional-rule (token)
  "Make a rule for a possibly missing (non)terminal (? syntax) and
return it."
  (insert-rule-in-current-grammar
   (gensym (concatenate 'string "OPT-"
			(if (rule-p token)
			    (symbol-name (rule-name token))
			    (string-upcase token))))
   `(((,token)) (()))))

(defun make-alternative-rule (tokens)
  "Make a rule for a list of alternatives (\"or\" syntax) and return it."
  (insert-rule-in-current-grammar
   (gensym "ALT")
   (mapcar #'(lambda (alternative)
	       `((,alternative)))
	   tokens)))

(defun make-nonempty-list-rule (token &optional separator)
  "Make a rule for a non-empty list (+ syntax) and return it."
  (let ((rule-name (gensym (concatenate 'string "NELST-"
					(if (rule-p token)
					    (symbol-name (rule-name token))
					    (string-upcase token))))))
    (insert-rule-in-current-grammar
     rule-name
     (if separator
	 `(((,token ,separator ,rule-name)
	    (cons $1 $3))
	   ((,token) ,#'list))
	 `(((,token ,rule-name)
	    (cons $1 $2))
	   ((,token) ,#'list))))))

(defun make-list-rule (token &optional separator)
  "Make a rule for a possibly empty list (* syntax) return it."
  (make-optional-rule (make-nonempty-list-rule token separator)))

(defun const-terminal-p (object)
  (or (stringp object)
      (keywordp object)))

(defun expand-production-token (tok)
  "Translate token of the type NAME? or NAME* or NAME+ into (? NAME)
or (* NAME) or (+ NAME).  This is used by the DEFRULE macro."
  (if (symbolp tok)
      (let* ((name (symbol-name tok))
	     (last (char name (1- (length name))))
	     ;; this looks silly but we need to make sure that we
	     ;; return symbols interned in this package, no one else
	     (op (cadr (assoc last '((#\? ?) (#\+ +) (#\* *))))))
	(if (and (> (length name) 1) op)
	    (list op
		  (intern (subseq name 0 (1- (length name)))))
	    tok))
      tok))

(defun EBNF-to-SEBNF (tokens)
  "Take a production as a list of TOKENS and expand it.  This turns a
EBNF syntax into a sexp-based EBNF syntax or SEBNF."
  (loop
     for tok in tokens
     for token = (expand-production-token tok)
     with new-tokens = '()
     do (cond ((member token '(* + ?))
	       (setf (car new-tokens)
		     (list token (car new-tokens))))
	      (t
	       (push token new-tokens)))
     finally (return (nreverse new-tokens))))

(defun SEBNF-to-BNF (tokens)
  "Take a production in SEBNF (Symbolic Extended BNF) syntax and turn
it into BNF.  The production is simplified but the current grammar is
populated with additional rules."
  (flet ((make-complex-token-rule (tok)
	   (ecase (car tok)
	     (* (apply #'make-list-rule (cdr tok)))
	     (+ (apply #'make-nonempty-list-rule (cdr tok)))
	     (? (make-optional-rule (cadr tok)))
	     (or (make-alternative-rule (cdr tok))))))
    (loop
       for token in tokens
       with new-tokens = '()
       with keywords = '()
       do (cond ((listp token)
		 (push (make-complex-token-rule token) new-tokens))
		(t
		 (push token new-tokens)
		 (when (const-terminal-p token)
		   (push token keywords))))
       finally (return (values (nreverse new-tokens) keywords)))))

(defun make-default-action-function (name tokens)
  "Create a sexp to be used as default action in case one is not
supplied in the production.  This is usually a quite sensible
one.  That is, only the non-constant tokens are returned in a
list and in case only a variable token is available that one is
returned (not included in a list).  If all the tokens are
constant, then all of them are returned in a list."
  (cond ((null tokens)
	 ;; if the production matched the empty list (no tokens) we
	 ;; return always nil, that is the function LIST applied to no
	 ;; arguments
	 #'list)
	((null (cdr tokens))
	 ;; if the production matches just one token we simply return
	 ;; that
	 #'identity)
	(*smart-default-reduction*
	 ;; If we are required to be "smart" then create a function
	 ;; that simply returns the non static tokens of the
	 ;; production.  If the production doesn't have nonterminal,
	 ;; then return all the tokens.  If the production has only
	 ;; one argument then return that one only.
	 (make-action-function name tokens '(cond
					     ((null $vars) $all)
					     ((null (cdr $vars)) (car $vars))
					     (t $vars))))
	(t
	 ;; in all the other cases we return all the token matching
	 ;; the production
	 #'list)))

(defun make-production-from-descr (name production-description)
  "Take a production NAME and its description in the form of a sexp
and return a production structure object together with a list of used
keywords."
  (destructuring-bind (tokens &optional action) production-description
    (let ((expanded-tokens (EBNF-to-SEBNF tokens)))
      (multiple-value-bind (production-tokens keywords)
	  (sebnf-to-bnf expanded-tokens)
      (let ((funct
	     (cond ((not action)
		    (make-default-action-function name expanded-tokens))
		   ((or (listp action)
			;; the case when the action is simply to
			;; return a token (ie $2) or a constant value
			(symbolp action))
		    (make-action-function name expanded-tokens action))
		   ((functionp action)
		    action)
		   (t			; action is a constant
		    #'(lambda (&rest args)
			(declare (ignore args))
			action)))))
	(values
	 ;; Make a promise instead of actually resolving the
	 ;; nonterminals.  This avoids endless recursion.
	 (make-production :tokens production-tokens
			  :tokens-length (length production-tokens)
			  :action funct)
	 keywords))))))

(defun remove-immediate-left-recursivity (rule)
  "Turn left recursive rules of the type
    A -> A x | y
into
    A -> y A2
    A2 -> x A2 | E
where E is the empty production."
  (let ((name (rule-name rule))
	(productions (rule-productions rule)))
    (loop
       for prod in productions
       for tokens = (prod-tokens prod)
       ;; when immediately left recursive
       when (eq (car tokens) rule)
       collect prod into left-recursive
       else
       collect prod into non-left-recursive
       finally
	 ;; found any left recursive production?
	 (when left-recursive
	   (warn "rule ~S is left recursive" name)
	   (let ((new-rule (make-rule :name (gensym "REWRITE"))))
	     ;; A -> y A2
	     (setf (rule-productions rule)
		   (mapcar #'(lambda (p)
			       (let ((tokens (prod-tokens p))
				     (action (prod-action p)))
				 (make-production :tokens (append tokens (list new-rule))
						  :tokens-length (1+ (prod-tokens-length p))
						  :action #'(lambda (&rest args)
							      (let ((f-A2 (car (last args)))
								    (head (butlast args)))
								(funcall f-A2 (apply action head)))))))
			   non-left-recursive))
	     ;; A2 -> x A2 | E
	     (setf (rule-productions new-rule)
		   (append
		    (mapcar #'(lambda (p)
				(let ((tokens (prod-tokens p))
				      (action (prod-action p)))
				  (make-production :tokens (append (cdr tokens) (list new-rule))
						   :tokens-length (prod-tokens-length p)
						   :action #'(lambda (&rest args)
							       (let ((f-A2 (car (last args)))
								     (head (butlast args)))
								 #'(lambda (x)
								     (funcall f-A2 (apply action x head))))))))
			    left-recursive)
		    (list
		     (make-production :tokens nil
				      :tokens-length 0
				      :action #'(lambda () #'(lambda (arg) arg)))))))))))

(defun remove-left-recursivity-from-rules (rules)
  (loop
     for rule being each hash-value in rules
     do
     ;; More to be done here.  For now only the trivial immediate left
     ;; recursivity is removed -wcp18/11/03.
       (remove-immediate-left-recursivity rule)))

(defun resolve-all-nonterminals (rules)
  (loop
     for rule being each hash-value in rules
     do (loop
	   for production in (rule-productions rule)
	   do (setf (prod-tokens production)
		    (resolve-nonterminals (prod-tokens production) rules)))))

(defun make-rule-productions (rule-name production-descriptions)
  "Return a production object that belongs to RULE-NAME made according
to PRODUCTION-DESCRIPTIONS.  See also MAKE-PRODUCTION-FROM-DESCR."
  (loop
     for descr in production-descriptions
     for i of-type fixnum from 1 by 1
     for prod-name = (intern (format nil "~:@(~A~)-PROD~A" rule-name i))
     with productions = '()
     with keywords = '()
     do (progn
	  (multiple-value-bind (production keyws)
	      (make-production-from-descr prod-name descr)
	    (push production productions)
	    (setf keywords (append keyws keywords))))
     finally (return
	       (values (nreverse productions) keywords))))

(defun create-rule (name production-descriptions)
  "Return a new rule object together with a list of keywords making up
the production definitions."
  (multiple-value-bind (productions keywords)
      (make-rule-productions name production-descriptions)
    (values (make-rule :name name :productions productions)
	    keywords)))

(defun insert-rule-in-current-grammar (name productions)
  "Add rule to the current grammar and its keywords to the keywords
hash table.  You don't want to use this directly.  See DEFRULE macro
instead."
  (when (find-rule name *rules*)
    (error "redefining rule ~A" name))
  (multiple-value-bind (rule keywords)
      (create-rule name productions)
    (add-rule name rule *rules*)
    (dolist (term keywords)
      (add-keyword term *keywords*))
    rule))

(defun resolve-nonterminals (tokens rules)
  "Given a list of production tokens, try to expand the nonterminal
ones with their respective rule from the the RULES pool."
  (flet ((resolve-symbol (sym)
	   (or (find-rule sym rules)
	       sym)))
    (mapcar #'(lambda (tok)
		(if (symbolp tok)
		    (resolve-symbol tok)
		    tok))
	    tokens)))

(defun reset-grammar ()
  "Empty the current grammar from any existing rule."
  (setf *rules* (make-rules-table)
	*keywords* (make-keywords-table)))

(defun generate-grammar (&optional (equal-p #'string-equal))
  "Return a GRAMMAR structure suitable for the PARSE function, using
the current rules.  EQUAL-P, if present, is a function to be used to
match the input tokens; it defaults to STRING-EQUAL."
  (resolve-all-nonterminals *rules*)
  (remove-left-recursivity-from-rules *rules*)
  (make-grammar :rules *rules*
		:keywords *keywords*
		:equal-p equal-p))
