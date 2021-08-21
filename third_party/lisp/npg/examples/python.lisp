;;;  python.lisp --- sample grammar definition for the Python language

;;;  Copyright (C) 2003 by Walter C. Pelissero

;;;  Author: Walter C. Pelissero <walter@pelissero.de>
;;;  Project: NPG a Naive Parser Generator
;;;  $Id: F-C1A8CD5961889C584B22F05E8B956006.lisp,v 1.3 2004/03/09 10:33:06 wcp Exp $

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
;;; This is far from being a complete Python grammar.  Actually I
;;; haven't even read a Python book before starting to write this
;;; stuff, so the code below comes mostly from wild guessing while
;;; reading a Python source file.
;;;
;;; It's a design decision to avoid writing any transformation in this
;;; module; only tagging is done at this level.  This improves the
;;; separation between parsing and transformation, making the grammar
;;; reusable for other purposes.


#+cmu (ext:file-comment "$Id: F-C1A8CD5961889C584B22F05E8B956006.lisp,v 1.3 2004/03/09 10:33:06 wcp Exp $")

(in-package :grammar)

(deflazy define-grammar
  (let ((*package* #.*package*)
	(*compile-print* (and parser::*debug* t)))
    (reset-grammar)
    (format t "~&creating Python grammar...~%")
    (populate-grammar)
    (let ((grammar (parser:generate-grammar)))
      (reset-grammar)
      (parser:print-grammar-figures grammar)
      grammar)))

(defun populate-grammar ()

(defrule program
    := comment-string? statement+)

(defrule comment-string
    := string eol
    :reduce string)

;;; BOB = Beginning Of Block, EOB = End Of Block.  It's lexical
;;; analyzer's task to find out where a statement or block starts/ends.

(defrule suite
    := statement-list eol
    :reduce statement-list
    := statement-block)

(defrule commentable-suite
    := statement-list eol
    :reduce statement-list
    := commented-statement-block)

(defrule statement-block
    := bob statement+ eob
    :reduce $2)

(defrule commented-statement-block
    := bob comment-string? statement* eob
    :reduce (cons comment-string statement))

(defrule statement-list
    := (+ simple-statement ";")
    :reduce (if (cdr $1)
		(cons :statement-list $1)
		(car $1)))

(defrule statement
    := statement-list eol
    :reduce statement-list
    := compound-statement)

(defrule simple-statement
    := import-statement
    := raise-statement
    := assignment
    := function-call
    := return-statement
    := assert-statement
    := pass-statement
    := break-statement
    := continue-statement)

(defrule compound-statement
    := class-definition
    := method-definition
    := try-statement
    := if-statement
    := while-statement
    := for-statement)

(defrule import-statement
    := "import" (+ package-name ",")
    :tag :import
    := "from" package-name "import" (+ symbol-name ",")
    :tag :import-from)

(defrule package-name := identifier)

(defrule symbol-name
    := identifier
    := "*")

(defrule try-statement
    := "try" ":" suite try-except-part* try-finally-part?
    :tag :try)

(defrule try-except-part
    := "except" exception-subject? ":" suite)

(defrule try-finally-part
    := "finally" ":" suite)

(defrule exception-subject
    := exception-name exception-variable?)

(defrule exception-variable
    := "," identifier)

(defrule exception-name := class-name)

(defrule class-name := identifier)

(defrule raise-statement
    := "raise"
    :tag :raise-same
    := "raise" exception-name
    :tag :raise
    := "raise" exception-name "," expression
    :tag :raise
    := "raise" exception-name "(" expression ")"
    :tag :raise)

(defrule assignment
    := (+ variable-with-optional-subscript ",") "=" more-assignment
    :tag :set)

(defrule more-assignment
    := expression
    := assignment)

(defrule variable-with-optional-subscript
    := variable-name subscript
    :tag :subscript
    := variable-name)

(defrule variable-name
    := (+ identifier ".")
    :tag :varef)

(defrule expression
    := expression "or" expression1
    :tag :or
    := expression1)

(defrule expression1
    := expression1 "and" expression2
    :tag :and
    := expression2)

(defrule expression2
    := expression2 "==" expression3
    :tag :equal
    := expression2 ">=" expression3
    :tag :more-equal
    := expression2 "<=" expression3
    :tag :less-equal
    := expression2 "!=" expression3
    :tag :not-equal
    := expression2 ">" expression3
    :tag :more
    := expression2 "<" expression3
    :tag :less
    := expression2 "is" expression3
    :tag :equal
    := expression2 "is" "not" expression3
    :tag :not-equal
    := expression3)

(defrule expression3
    := expression3 "+" expression4
    :tag :plus
    := expression3 "-" expression4
    :tag :minus
    := expression3 "|" expression4
    :tag :bit-or
    := expression4)

;; high priority expression
(defrule expression4
    := expression4 "*" expression5
    :tag :mult
    := expression4 "/" expression5
    :tag :div
    := expression4 "%" expression5
    :tag :modulo
    := expression4 "&" expression5
    :tag :bit-and
    := expression4 "in" expression5
    :tag :in
    := expression5)

(defrule expression5
    := "~" expression5
    :tag :bit-not
    := "not" expression5
    :tag :not
    := "(" expression ")"
    := expression6)

(defrule expression6
    := simple-expression subscript
    :tag :subscript
    := simple-expression)

(defrule simple-expression
    := function-call
    := variable-name
    := constant
    := string-conversion
    := list-constructor)

(defrule subscript
    := "[" expression "]"
    := "[" expression ":" expression "]"
    := "[" expression ":" "]"
    :reduce (list expression nil)
    := "[" ":" expression "]"
    :reduce (list nil expression))

(defrule string-conversion
    := "`" expression "`"
    :tag :to-string)

(defrule constant
    := number
    := string
    := lambda-expression)

(defrule number
    := float
    := integer)

(defrule list-constructor
    := "[" (* expression ",") "]"
    :tag :make-list)

(defrule class-definition
    := "class" class-name superclasses? ":" commentable-suite
    :tag :defclass)

(defrule superclasses
    := "(" class-name+ ")")

(defrule method-definition
    := "def" method-name "(" method-arguments ")" ":" commentable-suite
    :tag :defmethod)

(defrule method-arguments
    := (* method-argument ","))

(defrule method-argument
    := identifier argument-default?)

(defrule argument-default
    := "=" expression)

(defrule method-name := identifier)

(defrule if-statement
    := "if" expression ":" suite elif-part* else-part?
    :tag :if)

(defrule else-part
    :=  "else" ":" suite)

(defrule elif-part
    := "elif" expression ":" suite)

(defrule lambda-expression
    := "lambda" method-arguments ":" expression
    :tag :lambda)

(defrule function-call
    := (+ identifier ".") "(" (* expression ",") ")"
    :tag :funcall)

(defrule for-statement
    := "for" identifier "in" expression ":" suite
    :tag :do-list
    := "for" identifier "in" "range" "(" expression "," expression ")" ":" suite
    :tag :do-range)

(defrule while-statement
    := "while" expression ":" suite
    :tag :while)

(defrule return-statement
    := "return" expression?
    :tag :return)

(defrule assert-statement
    := "assert" expression "," string
    :tag :assert)

(defrule pass-statement
    := "pass"
    :tag :pass)

(defrule break-statement
    := "break"
    :tag :break)

(defrule continue-statement
    := "continue"
    :tag :continue)

)					; end of POPULATE-GRAMMAR
