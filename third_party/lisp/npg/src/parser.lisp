;;;  parser.lisp --- runtime parser

;;;  Copyright (C) 2003-2006, 2009 by Walter C. Pelissero

;;;  Author: Walter C. Pelissero <walter@pelissero.de>
;;;  Project: NPG a Naive Parser Generator

#+cmu (ext:file-comment "$Module: parser.lisp $")

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

;;;  Commentary:
;;;
;;; This is the runtime part of the parser.  The code that is
;;; responsible to execute the parser defined with the primitives
;;; found in define.lisp.

(in-package :naive-parser-generator)

(defvar *debug* nil
  "Either nil or a stream where to write the debug informations.")
#+debug (declaim (fixnum *maximum-recursion-depth*))
#+debug (defvar *maximum-recursion-depth* 1000
  "Maximum depth the parser is allowed to recursively call itself.
This is the only way for the parser to detect a loop in the grammar.
Tune this if your grammar is unusually complex.")

(declaim (inline reduce-production))
(defun reduce-production (production arguments)
  "Apply PRODUCTION's action on ARGUMENTS.  This has the effect of
  \"reducing\" the production."
  (when *debug*
    (format *debug* "reducing ~S on ~S~%" production arguments))
  (flet ((safe-token-value (token)
	   (if (token-p token)
	       (token-value token)
	       token)))
    (apply (prod-action production) (mapcar #'safe-token-value arguments))))

(defgeneric later-position (pos1 pos2)
  (:documentation
   "Compare two file postions and return true if POS1 is later than
POS2 in the input stream."))

;; This is meant to be overloaded in the lexer
(defmethod later-position ((pos1 integer) (pos2 integer))
  (> pos1 pos2))

;; this looks silly but turns out to be useful (see below)
(defmethod later-position (pos1 pos2)
  (and (eq pos1 :eof) (not (eq pos2 :eof))))

(defgeneric read-next-tokens (tokens-source)
  (:documentation "Read next token from a lexical analysed stream.  The nature of
TOKENS-SOURCE is implementation dependent and any lexical analyzer is
supposed to specialise this method."))

;; This is the actual parser.  the algorithm is pretty
;; straightforward, the execution of the reductions a bit less.  Error
;; recovery is rather clumsy.

(defun parse (grammar start tokenizer)
  "Match a GRAMMAR against the list of input tokens coming from TOKENIZER.
Return the reduced values according to the nonterminal actions.  Raise
an error on failure."
  (declare (type grammar grammar)
	   (type symbol start))
  (labels
      ((match-token (expected token)
	 (when *debug*
	   (format *debug* "match-token ~S ~S -> " expected token))
	 (let ((res (cond ((symbolp expected)
			   ;; non-costant terminal (like identifiers)
			   (eq expected (token-type token)))
			  ((and (stringp expected)
				(stringp (token-value token)))
			   ;; string costant terminal
			   (funcall (the function (grammar-equal-p grammar)) expected (token-value token)))
			  ((functionp expected)
			   ;; custom equality predicate (must be able
			   ;; to deal with token objects)
			   (funcall expected token))
			  ;; all the rest
			  (t (equal expected (token-value token))))))
	   (when *debug*
	     (format *debug* "~Amatched~%" (if res "" "not ")))
	   res))
       (match (expected matched #+debug depth)
	 (declare (list expected matched)
		  #+debug (fixnum depth))
	 (let ((first-expected (car expected)))
	   (cond #+debug ((> depth *maximum-recursion-depth*)
		  (error "endless recursion on ~A ~A at ~A expecting ~S"
			 (token-type (car matched)) (token-value (car matched))
			 (token-position (car matched)) expected))
		 ((eq first-expected :any)
		  (match (cdr expected) (cdr matched) #+debug depth))
		 ;; This is a trick to obtain partial parses.  When we
		 ;; reach this expected token we assume we succeeded
		 ;; the parsing and return the remaining tokens as
		 ;; part of the match.
		 ((eq first-expected :rest)
		  ;; we could be at the end of input so we check this
		  (unless (cdr matched)
		    (setf (cdr matched) (list :rest)))
		  (list nil nil))
		 ((rule-p first-expected)
		  ;; If it's a rule, then we try to match all its
		  ;; productions.  We return the first that succeeds.
		  (loop
		     for production in (rule-productions first-expected)
		     for production-tokens of-type list = (prod-tokens production)
		     with last-error-position = nil
		     with last-error = nil
		     for (error-position error-descr) =
		       (progn
			 (when *debug*
			   (format *debug* "trying to match ~A: ~S~%"
				   (rule-name first-expected) production-tokens))
			 (match (append production-tokens (cdr expected)) matched #+debug (1+ depth)))
		     do (cond ((not error-position)
			       (return (let ((args-count (prod-tokens-length production)))
					 (setf (cdr matched)
					       (cons (reduce-production
						      production
						      (subseq (the list (cdr matched)) 0 args-count))
						     (nthcdr (1+ args-count) matched)))
					 (list nil nil))))
			      ((or (not last-error)
				   (later-position error-position last-error-position))
			       (setf last-error-position error-position
				     last-error error-descr)))
		     ;; if everything fails return the "best" error
		     finally (return (list last-error-position
					   (if *debug*
					       #'(lambda ()
						   (format nil "~A, trying to match ~A"
							   (funcall (the function last-error))
							   (rule-name first-expected)))
					       last-error)))))
		 (t
		  ;; if necessary load the next tokens
		  (when (null (cdr matched))
		    (setf (cdr matched) (read-next-tokens tokenizer)))
		  (cond ((and (or (null expected) (eq first-expected :eof))
			      (null (cdr matched)))
			 ;; This point is reached only once for each complete
			 ;; parsing.  The expected tokens and the input
			 ;; tokens have been exhausted at the same time.
			 ;; Hence we succeeded the parsing.
			 (setf (cdr matched) (list :eof))
			 (list nil nil))
			((null expected)
			 ;; Garbage at end of parsing.  This may mean that we
			 ;; have considered a production completed too soon.
			 (list (token-position (car matched))
			       #'(lambda ()
				   "garbage at end of parsing")))
			((null (cdr matched))
			 ;; EOF error
			 (list :eof
			       #'(lambda ()
				   (format nil "end of input expecting ~S" expected))))
			(t ;; normal token
			 (let ((first-token (cadr matched)))
			   (if (match-token first-expected first-token)
			       (match (cdr expected) (cdr matched) #+debug depth)
			       ;; failed: we return the error
			       (list (token-position first-token)
				     #'(lambda ()
					 (format nil "expected ~S but got ~S ~S"
						 first-expected (token-type first-token)
						 (token-value first-token)))))))))))))
    (declare (inline match-token))
    (let ((result (list :head)))
      (destructuring-bind (error-position error)
	  (match (list (find-rule start (grammar-rules grammar))) result #+debug 0)
	(when error-position
	  (error "~A at ~A~%" (funcall (the function error)) error-position))
	(cadr result)))))

(defgeneric terminals-in-grammar (grammar-or-hashtable)
  (:documentation
   "Find non constant terminal symbols in GRAMMAR."))

(defmethod terminals-in-grammar ((grammar hash-table))
  (loop
     for rule being each hash-value of grammar
     with terminals = '()
     do (loop
	   for prod in (rule-productions rule)
	   do (loop
		 for tok in (prod-tokens prod)
		 when (symbolp tok)
		 do (pushnew tok terminals)))
     finally (return terminals)))

(defmethod terminals-in-grammar ((grammar grammar))
  (terminals-in-grammar (grammar-rules grammar)))

(defun print-grammar-figures (grammar &optional (stream *standard-output*))
  (format stream "rules: ~A~%constant terminals: ~A~%variable terminals: ~S~%"
	  (hash-table-count (grammar-rules grammar))
	  (hash-table-count (grammar-keywords grammar))
	  (terminals-in-grammar (grammar-rules grammar))))


(defun grammar-keyword-p (keyword grammar)
  "Check if KEYWORD is part of this grammar."
  (find-keyword keyword (grammar-keywords grammar)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *grammars* (make-hash-table))

(defun find-grammar (name)
  (gethash name *grammars*))

(defun delete-grammar (name)
  (remhash name *grammars*))

(defun add-grammar (name grammar)
  (setf (gethash name *grammars*) grammar))
