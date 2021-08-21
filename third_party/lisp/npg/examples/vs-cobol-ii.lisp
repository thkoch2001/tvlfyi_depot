;;;  vs-cobol-ii.lisp --- sample grammar for VS-Cobol II

;;;  Copyright (C) 2003 by Walter C. Pelissero

;;;  Author: Walter C. Pelissero <walter@pelissero.de>
;;;  Project: NPG a Naive Parser Generator
;;;  $Id: F-1D03709AEB30BA7644C1CFA2DF60FE8C.lisp,v 1.2 2004/03/09 10:33:07 wcp Exp $

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
;;; A fairly incomplete VS-Cobol II grammar fro NPG.  It's probably
;;; not very accurate either.

#+cmu (ext:file-comment "$Id: F-1D03709AEB30BA7644C1CFA2DF60FE8C.lisp,v 1.2 2004/03/09 10:33:07 wcp Exp $")

(in-package :grammar)

(defun make-keyword (string)
  "Create a keyword from STRING."
  (intern (string-upcase string) :keyword))

(defun flatten-list (list)
  "Remove one depth level in LIST."
  (mapcan #'identity list))

(deflazy define-grammar
  (let ((*package* #.*package*)
	(*compile-print* (and parser::*debug* t)))
    (reset-grammar)
    (format t "creating Cobol grammar...~%")
    (populate-grammar)
    (let ((grammar (parser:generate-grammar)))
      (reset-grammar)
      (parser:print-grammar-figures grammar)
      grammar)))

(defun populate-grammar ()
;;;
;;; Hereafter PP means Partial Program
;;;

#+nil
(defrule pp--declarations
    := identification-division environment-division? data-division? "PROCEDURE" "DIVISION" using-phrase? "." :rest)

;;; We need to split the parsing of the declarations from the rest
;;; because the declarations may change the lexical rules (ie decimal
;;; point)

(defrule pp--declarations
    := identification-division environment-division? data-division-head-or-procedure-division-head :rest)

(defrule data-division-head-or-procedure-division-head
    := data-division-head
    :reduce :data-division
    := procedure-division-head
    :reduce (list :procedure-division $1))

(defrule pp--data-division
    := data-division-content procedure-division-head :rest)

(defrule pp--sentence
    := sentence :rest
    := :eof)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The real grammar
;;;

(defrule cobol-source-program
    := identification-division environment-division? data-division procedure-division end-program?)

(defrule identification-division
    := identification "DIVISION" "." program-id-cobol-source-program identification-division-content
    :reduce program-id-cobol-source-program)

(defrule priority-number
    := integer)

(defrule level-number
    := integer)

(defrule to-id-or-lit
    := "TO" id-or-lit)

(defrule inspect-by-argument
    := variable-identifier
    := string
    := figurative-constant-simple)

(defrule figurative-constant-simple
    := "ZERO"
    :reduce :zero
    := "ZEROS"
    :reduce :zero
    := "ZEROES"
    :reduce :zero
    := "SPACE"
    :reduce :space
    := "SPACES"
    :reduce :space
    := "HIGH-VALUE"
    :reduce :high
    := "HIGH-VALUES"
    :reduce :high
    := "LOW-VALUE"
    :reduce :low
    := "LOW-VALUES"
    :reduce :low
    := "QUOTE"
    :reduce :quote
    := "QUOTES"
    :reduce :quote
    := "NULL"
    :reduce :null
    := "NULLS"
    :reduce :null)

(defrule write-exceptions
    := at-end-of-page-statement-list? not-at-end-of-page-statement-list? invalid-key-statement-list? not-invalid-key-statement-list?)

(defrule set-statement-phrase
    := variable-identifier+ set-oper set-src)

(defrule set-src
    := variable-identifier
    := literal
    := "TRUE"
    := "ON"
    := "OFF")

(defrule set-oper
    := "TO"
    :reduce :to
    := "UP" "BY"
    :reduce :up
    := "DOWN" "BY"
    :reduce :down)

(defrule fce-phrase
    := reserve-clause
    := fce-organization
    := fce-access-mode
    := record-key-clause
    := password-clause
    := alternate-record-key-clause
    := file-status-clause
    := padding-character-clause
    := record-delimiter-clause)

(defrule fce-organization
    := organization-is? alt-indexed-relative-sequential
    :reduce (list :organization (make-keyword alt-indexed-relative-sequential)))

(defrule fce-access-mode
    := "ACCESS" "MODE"? "IS"? alt-sequential-random-dynamic relative-key-clause?
    :reduce (list :access-mode (make-keyword alt-sequential-random-dynamic)))

(defrule alt-indexed-relative-sequential
    := "INDEXED"
    := "RELATIVE"
    := "SEQUENTIAL")

(defrule is-not
    := "IS"? "NOT"?)

(defrule all-procedures
    := "ALL" "PROCEDURES")

(defrule next-sentence
    := "NEXT" "SENTENCE")

(defrule no-rewind
    := "NO" "REWIND")

(defrule for-removal
    := "FOR"? "REMOVAL")

(defrule values
    := "VALUE"
    := "VALUES")

(defrule records
    := "RECORD"
    := "RECORDS")

(defrule end-program
    := "END" "PROGRAM" program-name ".")

(defrule environment-division
    := "ENVIRONMENT" "DIVISION" "." environment-division-content)

(defrule data-division-head
    := "DATA" "DIVISION" ".")

(defrule data-division
    := data-division-head data-division-content
    :reduce data-division-content)

(defrule identification
    := "IDENTIFICATION"
    := "ID")

(defrule identification-division-content
    := identification-division-phrase*)

(defrule author
    := "AUTHOR" ".")

(defrule installation
    := "INSTALLATION" ".")

(defrule date-written
    := "DATE-WRITTEN" ".")

(defrule date-compiled
    := "DATE-COMPILED" ".")

(defrule security
    := "SECURITY" ".")

(defrule remarks
    := "REMARKS" ".")

(defrule identification-division-phrase
    := author
    := installation
    := date-written
    := date-compiled
    := security
    := remarks)

(defrule program-id-cobol-source-program
    := "PROGRAM-ID" "."? program-name initial-program? "."
    :reduce program-name)

(defrule initial-program
    := "IS"? "INITIAL" "PROGRAM"?)

(defrule environment-division-content
    := configuration-section? input-output-section?)

(defrule input-output-section
    := "INPUT-OUTPUT" "SECTION" "." file-control-paragraph? i-o-control-paragraph?
    :reduce file-control-paragraph)

(defrule file-control-paragraph
    := "FILE-CONTROL" "." file-control-entry*)

(defrule file-control-entry
    := select-clause assign-clause fce-phrase* "."
    :reduce (append select-clause
		    assign-clause
		    (flatten-list fce-phrase)))

(defrule organization-is
    := "ORGANIZATION" "IS"?)

(defrule alt-sequential-random-dynamic
    := "SEQUENTIAL"
    := "RANDOM"
    := "DYNAMIC")

(defrule select-clause
    := "SELECT" "OPTIONAL"? file-name
    :reduce (list file-name :optional (and $2 t)))

(defrule assign-clause
    := "ASSIGN" "TO"? alt-assignment-name-literal+
    :reduce (list :assign alt-assignment-name-literal))

(defrule alt-assignment-name-literal
    := assignment-name
    := literal)

(defrule reserve-clause
    := "RESERVE" integer areas?)

(defrule areas
    := "AREA"
    := "AREAS")

(defrule padding-character-clause
    := "PADDING" "CHARACTER"? "IS"? alt-qualified-data-name-literal)

(defrule record-delimiter-clause
    := "RECORD" "DELIMITER" "IS"? record-delimiter-name)

(defrule record-delimiter-name
    := "STANDARD-1"
    := assignment-name)

(defrule password-clause
    := "PASSWORD" "IS"? data-name)

(defrule file-status-clause
    := "FILE"? "STATUS" "IS"? qualified-data-name qualified-data-name?
    :reduce (list :file-status qualified-data-name))

(defrule relative-key-clause
    := "RELATIVE" "KEY"? "IS"? qualified-data-name
    :reduce (list :relative-key qualified-data-name))

(defrule record-key-clause
    := "RECORD" "KEY"? "IS"? qualified-data-name
    :reduce (list :key qualified-data-name))

(defrule alternate-record-key-clause
    := "ALTERNATE" "RECORD"? "KEY"? "IS"? qualified-data-name password-clause? with-duplicates?
    :reduce (list :alternate-key qualified-data-name with-duplicates))

(defrule with-duplicates
    := "WITH"? "DUPLICATES")

(defrule i-o-control-paragraph
    := "I-O-CONTROL" "." i-o-sam? i-o-sort-merge?)

(defrule i-o-sam
    := qsam-or-sam-or-vsam-i-o-control-entries+ ".")

(defrule i-o-sort-merge
    := sort-merge-i-o-control-entries ".")

(defrule qsam-or-sam-or-vsam-i-o-control-entries
    := qsam-or-sam-or-vsam-i-o-control-entries-1
    := qsam-or-sam-or-vsam-i-o-control-entries-2
    := qsam-or-sam-or-vsam-i-o-control-entries-3
    := qsam-or-sam-or-vsam-i-o-control-entries-4)

(defrule qsam-or-sam-or-vsam-i-o-control-entries-1
    := "RERUN" "ON" alt-assignment-name-file-name "EVERY"? every-phrase "OF"? file-name)

(defrule every-phrase-1
    := integer "RECORDS")

(defrule every-phrase-2
    := "END" "OF"? alt-reel-unit)

(defrule every-phrase
    := every-phrase-1
    := every-phrase-2)

(defrule alt-assignment-name-file-name
    := assignment-name
    := file-name)

(defrule qsam-or-sam-or-vsam-i-o-control-entries-2
    := "SAME" "RECORD"? "AREA"? "FOR"? file-name file-name+)

(defrule qsam-or-sam-or-vsam-i-o-control-entries-3
    := "MULTIPLE" "FILE" "TAPE"? "CONTAINS"? file-name-position+)

(defrule position
    := "POSITION" integer)

(defrule file-name-position
    := file-name position?)

(defrule qsam-or-sam-or-vsam-i-o-control-entries-4
    := "APPLY" "WRITE-ONLY" "ON"? file-name+)

(defrule sort-merge-i-o-control-entries
    := rerun-on? same-area+)

(defrule rerun-on
    := "RERUN" "ON" assignment-name)

(defrule record-sort
    := "RECORD"
    := "SORT"
    := "SORT-MERGE")

(defrule same-area
    := "SAME" record-sort "AREA"? "FOR"? file-name file-name+)

(defrule configuration-section
    := "CONFIGURATION" "SECTION" "." configuration-section-paragraph*
    :reduce (flatten-list configuration-section-paragraph))

(defrule configuration-section-paragraph
    := source-computer-paragraph
    := object-computer-paragraph
    := special-names-paragraph)

(defrule source-computer-paragraph
    := "SOURCE-COMPUTER" "." source-computer-name
    :reduce (list :source-computer source-computer-name))

(defrule with-debugging-mode
    := "WITH"? "DEBUGGING" "MODE")

(defrule source-computer-name
    := computer-name with-debugging-mode? "."
    :reduce computer-name)

(defrule object-computer-paragraph
    := "OBJECT-COMPUTER" "." object-computer-name
    :reduce (list :object-computer object-computer-name))

(defrule memory-size-type
    := "WORDS"
    := "CHARACTERS"
    := "MODULES")

(defrule memory-size
    := "MEMORY" "SIZE"? integer memory-size-type)

(defrule object-computer-name
    := computer-name memory-size? object-computer-paragraph-sequence-phrase "."
    :reduce computer-name)

(defrule object-computer-paragraph-sequence-phrase
    := program-collating-sequence? segment-limit?)

(defrule program-collating-sequence
    := "PROGRAM"? "COLLATING"? "SEQUENCE" "IS"? alphabet-name)

(defrule segment-limit
    := "SEGMENT-LIMIT" "IS"? priority-number)

(defrule special-names-paragraph
    := "SPECIAL-NAMES" "." special-names-paragraph-phrase* special-names-paragraph-clause* "."
    :reduce (flatten-list special-names-paragraph-clause))

(defrule is-mnemonic-name
    := "IS"? mnemonic-name special-names-paragraph-status-phrase?)

(defrule special-names-paragraph-phrase-tail
    := is-mnemonic-name
    := special-names-paragraph-status-phrase)

(defrule special-names-paragraph-phrase
    := environment-name special-names-paragraph-phrase-tail)

(defrule special-names-paragraph-status-phrase
    := special-names-paragraph-status-phrase-1
    := special-names-paragraph-status-phrase-2)

(defrule special-names-paragraph-status-phrase-1
    := "ON" "STATUS"? "IS"? condition off-status?)

(defrule off-status
    := "OFF" "STATUS"? "IS"? condition)

(defrule special-names-paragraph-status-phrase-2
    := "OFF" "STATUS"? "IS"? condition on-status?)

(defrule on-status
    := "ON" "STATUS"? "IS"? condition)

(defrule special-names-paragraph-clause
    ;; := alphabet-clause
    ;; := symbolic-characters-clause
    := currency-sign-clause
    := decimal-point-clause)

(defrule alphabet-clause
    := "ALPHABET" alphabet-name "IS"? alphabet-type)

(defrule alphabet-type-also
    := "ALSO" literal)

(defrule alphabet-type-alsos
    := alphabet-type-also+)

(defrule alphabet-type-also-through
    := through-literal
    := alphabet-type-alsos)

(defrule alphabet-type-other
    := literal alphabet-type-also-through?)

(defrule alphabet-type-others
    := alphabet-type-other+)

(defrule alphabet-type
    := "STANDARD-1"
    := "STANDARD-2"
    := "NATIVE"
    := "EBCDIC"
    := alphabet-type-others)

(defrule symbolic-characters-clause
    := "SYMBOLIC" "CHARACTERS"? symbolic-character-mapping+ in-alphabet-name?)

(defrule are
    := "ARE"
    := "IS")

(defrule symbolic-character-mapping
    := symbolic-character+ are? integer+)

(defrule in-alphabet-name
    := "IN" alphabet-name)

(defrule currency-sign-clause
    := "CURRENCY" "SIGN"? "IS"? literal
    :reduce (list :currency-sign literal))

(defrule decimal-point-clause
    := "DECIMAL-POINT" "IS"? "COMMA"
    :reduce (list :decimal-point #\,))

(defrule data-division-content
    := file-section? working-storage-section? linkage-section?)

(defrule file-section-entry
    := file-and-sort-description-entry data-description-entry+
    :reduce (cons file-and-sort-description-entry data-description-entry))

(defrule file-section-head
    := "FILE" "SECTION" ".")

(defrule file-section
    := file-section-head file-section-entry*
    :reduce $2)

(defrule working-storage-section-head
    := "WORKING-STORAGE" "SECTION" ".")

(defrule working-storage-section
    := working-storage-section-head data-description-entry*
    :reduce $2)

(defrule linkage-section-head
    := "LINKAGE" "SECTION" ".")

(defrule linkage-section
    := linkage-section-head data-description-entry*
    :reduce $2)

(defrule file-and-sort-description-entry
    := alt-fd-sd file-name file-and-sort-description-entry-clause* "."
    :reduce (list (make-keyword alt-fd-sd) file-name file-and-sort-description-entry-clause))

(defrule alt-fd-sd
    := "FD"
    := "SD")

(defrule file-and-sort-description-entry-clause
    := external-clause
    := global-clause
    := block-contains-clause
    := record-clause
    := label-records-clause
    := value-of-clause
    := data-records-clause
    := linage-clause
    := recording-mode-clause
    := code-set-clause)

(defrule integer-to
    := integer "TO")

(defrule block-contains-clause
    := "BLOCK" "CONTAINS"? integer-to? integer alt-characters-records?)

(defrule alt-characters-records
    := "CHARACTERS"
    := "RECORDS"
    := "RECORD")

(defrule record-clause
    := "RECORD" record-clause-tail)

(defrule depending-on
    := "DEPENDING" "ON"? data-name)

(defrule record-clause-tail-1
    := "CONTAINS"? integer "CHARACTERS"?)

(defrule record-clause-tail-2
    := "CONTAINS"? integer "TO" integer "CHARACTERS"?)

(defrule record-clause-tail-3
    := record-varying-phrase depending-on?)

(defrule record-clause-tail
    := record-clause-tail-2
    := record-clause-tail-1
    := record-clause-tail-3)

(defrule record-varying-phrase
    := "IS"? "VARYING" "IN"? "SIZE"? from-integer? to-integer? "CHARACTERS"?)

(defrule from-integer
    := "FROM"? integer)

(defrule to-integer
    := "TO" integer)

(defrule label-records-clause
    := "LABEL" records-are label-records-clause-tail
    :reduce (list :label-record label-records-clause-tail))

(defrule data-names
    := data-name+)

(defrule label-records-clause-tail
    := "STANDARD" :reduce :standard
    := "OMITTED" :reduce :omitted
    := data-names)

(defrule value-of-clause
    := "VALUE" "OF" value-of-clause-tail+)

(defrule alt-qualified-data-name-literal
    := qualified-data-name
    := literal)

(defrule value-of-clause-tail
    := variable-identifier "IS"? alt-qualified-data-name-literal)

(defrule data-records-clause
    := "DATA" records-are data-name+)

(defrule records-are
    := records are?)

(defrule linage-clause
    := "LINAGE" "IS"? alt-data-name-integer "LINES"? linage-footing-phrase)

(defrule linage-footing-phrase
    := footing? lines-top? lines-bottom?)

(defrule alt-data-name-integer
    := data-name
    := integer)

(defrule footing
    := "WITH"? "FOOTING" "AT"? alt-data-name-integer)

(defrule lines-top
    := "LINES"? "AT"? "TOP" alt-data-name-integer)

(defrule lines-bottom
    := "LINES"? "AT"? "BOTTOM" alt-data-name-integer)

(defrule recording-mode-clause
    := "RECORDING" "MODE"? "IS"? variable-identifier)

(defrule code-set-clause
    := "CODE-SET" "IS"? alphabet-name)

(defrule data-description-entry
    := level-number alt-data-name-filler? data-description-entry-clause* "."
    :reduce (append (list level-number alt-data-name-filler)
		    (flatten-list data-description-entry-clause)))

(defrule alt-data-name-filler
    := data-name
    := "FILLER"
    :reduce (list))

(defrule data-description-entry-clause
    := picture-clause
    := redefines-clause
    := blank-when-zero-clause
    := external-clause
    := global-clause
    := justified-clause
    := occurs-clause
    := sign-clause
    := synchronized-clause
    := usage-clause
    := renames-clause
    := value-clause)

(defrule value-clause
    := "VALUE" "IS"? literal
    :reduce (list :value literal))

(defrule redefines-clause
    := "REDEFINES" data-name
    :reduce `(:redefines ,data-name))

(defrule blank-when-zero-clause
    := "BLANK" "WHEN"? zeroes
    :reduce '(:blank-when-zero t))

(defrule zeroes
    := "ZERO"
    := "ZEROS"
    := "ZEROES")

(defrule external-clause
    := "IS"? "EXTERNAL"
    :reduce '(:external t))

(defrule global-clause
    := "IS"? "GLOBAL"
    :reduce '(:global t))

(defrule justified-clause
    := justified "RIGHT"?
    :reduce `(:justified ,(if $2 :right :left)))

(defrule justified
    := "JUSTIFIED"
    := "JUST")

(defrule occurs-clause
    := "OCCURS" integer "TIMES"? occurs-clause-key* indexed-by?
    ;; to be completed -wcp16/7/03.
    :reduce `(:times ,integer)
    := "OCCURS" integer "TO" integer "TIMES"? "DEPENDING" "ON"? qualified-data-name occurs-clause-key* indexed-by?
    ;; to be completed -wcp16/7/03.
    :reduce `(:times (,integer ,integer2 ,qualified-data-name)))

(defrule occurs-clause-key
    := alt-ascending-descending "KEY"? "IS"? qualified-data-name+)

(defrule indexed-by
    := "INDEXED" "BY"? index-name+)

(defrule picture-clause
    := picture "IS"? picture-string
    :reduce `(:picture ,picture-string))

(defrule picture
    := "PICTURE"
    := "PIC")

(defrule sign-clause
    := sign-is? alt-leading-trailing separate-character?
    :reduce `(:separate-sign ,separate-character :sign-position ,alt-leading-trailing))

(defrule sign-is
    := "SIGN" "IS"?)

(defrule separate-character
    := "SEPARATE" "CHARACTER"?
    :reduce t)

(defrule alt-leading-trailing
    := "LEADING"
    :reduce :leading
    := "TRAILING"
    :reduce :trailing)

(defrule synchronized-clause
    := synchronized alt-left-right?
    :reduce `(:synchronized ,(if alt-left-right
				 alt-left-right
				 t)))

(defrule alt-left-right
    := "LEFT"
    :reduce :left
    := "RIGHT"
    :reduce :right)

(defrule synchronized
    := "SYNCHRONIZED"
    := "SYNC")

(defrule usage-clause
    := usage-is? usage
    :reduce (list :encoding usage))

(defrule usage-is
    := "USAGE" "IS"?)

(defrule usage
    := "BINARY"
    :reduce :binary
    := "COMP"
    :reduce :comp
    := "COMP-1"
    :reduce :comp1
    := "COMP-2"
    :reduce :comp2
    := "COMP-3"
    :reduce :comp3
    := "COMP-4"
    :reduce :comp4
    := "COMPUTATIONAL"
    :reduce :comp
    := "COMPUTATIONAL-1"
    :reduce :comp1
    := "COMPUTATIONAL-2"
    :reduce :comp2
    := "COMPUTATIONAL-3"
    :reduce :comp3
    := "COMPUTATIONAL-4"
    :reduce :comp4
    := "DISPLAY"
    :reduce :display
    := "DISPLAY-1"
    :reduce :display1
    := "INDEX"
    :reduce :index
    := "PACKED-DECIMAL"
    :reduce :packed-decimal
    := "POINTER"
    :reduce :pointer)

(defrule renames-clause
    := "RENAMES" qualified-data-name through-qualified-data-name?
    :reduce `(:renames ,qualified-data-name ,through-qualified-data-name))

(defrule through-qualified-data-name
    := through qualified-data-name
    :reduce qualified-data-name)

(defrule condition-value-clause
    := values-are literal-through-literal+)

(defrule through-literal
    := through literal)

(defrule literal-through-literal
    := literal through-literal?)

(defrule values-are
    := values are?)

(defrule procedure-division-head
    := "PROCEDURE" "DIVISION" using-phrase? ".")

(defrule procedure-division
    := procedure-division-head sentence+)

(defrule using-phrase
    := "USING" data-name+)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defrule declaratives
    := "DECLARATIVES" "." declaratives-content+ "END" "DECLARATIVES" ".")

(defrule declaratives-content
    := cobol-identifier "SECTION" "." use-statement "." sentence*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defrule paragraph-header
    := cobol-identifier "SECTION"?
    :reduce (list (if $2 :section :label) $1))

(defrule sentence
    := declaratives
    := statement* "."
    :reduce $1
    := paragraph-header "."
    :reduce $1)

(defrule statement
    := move-statement
    := if-statement
    := perform-statement
    := go-to-statement
    := accept-statement
    := add-statement
    := alter-statement
    := call-statement
    := cancel-statement
    := close-statement
    := compute-statement
    := continue-statement
    := delete-statement
    := display-statement
    := divide-statement
    := entry-statement
    := evaluate-statement
    := exit-program-statement
    := exit-statement
    := goback-statement
    := initialize-statement
    := inspect-statement
    := merge-statement
    := multiply-statement
    := open-statement
    := read-statement
    := release-statement
    := return-statement
    := rewrite-statement
    := search-statement
    := set-statement
    := sort-statement
    := start-statement
    := stop-statement
    := string-statement
    := subtract-statement
    := unstring-statement
    := write-statement
    := paragraph-header)

(defrule accept-statement
    := "ACCEPT" variable-identifier "FROM" date
    := "ACCEPT" variable-identifier "AT" screen-coordinates
    :reduce (apply #'list 'accept-at variable-identifier screen-coordinates)
    := "ACCEPT" variable-identifier from-environment-name?)

(defrule from-environment-name
    := "FROM" cobol-identifier)


(defrule date
    := "DATE"
    := "DAY"
    := "DAY-OF-WEEK"
    := "TIME")

(defrule add-statement
    := "ADD" id-or-lit+ to-id-or-lit? "GIVING" cobword-rounded+ on-size-error-statement-list? not-on-size-error-statement-list? "END-ADD"?
    := "ADD" id-or-lit+ "TO" cobword-rounded+ on-size-error-statement-list? not-on-size-error-statement-list? "END-ADD"?
    := "ADD" corresponding variable-identifier "TO" variable-identifier "ROUNDED"? on-size-error-statement-list? not-on-size-error-statement-list? "END-ADD"?)

(defrule statement-list
    := statement+)

(defrule alter-statement
    := "ALTER" procedure-to-procedure+)

(defrule proceed-to
    := "PROCEED" "TO")

(defrule procedure-to-procedure
    := procedure-name "TO" proceed-to? procedure-name)

(defrule call-statement
    := "CALL" id-or-lit using-parameters? call-rest-phrase "END-CALL"?
    :reduce (list 'call id-or-lit (cons 'list using-parameters)))

(defrule by-reference
    := "BY"? "REFERENCE")

(defrule content-parameter-value
    := cobol-identifier
    := literal)

(defrule reference-parameter
    := by-reference? variable-identifier)

(defrule content-parameter
    := "BY"? "CONTENT" content-parameter-value+)

(defrule parameter
    := reference-parameter
    := content-parameter
    := literal)

(defrule using-parameters
    := "USING" parameter+)

(defrule call-rest-phrase
    := on-exception-statement-list? not-on-exception-statement-list? on-overflow-statement-list?)

(defrule on-exception-statement-list
    := "ON"? "EXCEPTION" statement-list)

(defrule not-on-exception-statement-list
    := "NOT" "ON"? "EXCEPTION" statement-list)

(defrule cancel-statement
    := "CANCEL" id-or-lit+)

(defrule close-statement
    := "CLOSE" close-statement-file-name+
    :reduce (list 'close close-statement-file-name))

(defrule alt-removal-no-rewind
    := for-removal
    := with-no-rewind)

(defrule alt-reel-unit
    := "REEL"
    := "UNIT")

(defrule alt-no-rewind-lock
    := no-rewind
    := "LOCK")

(defrule close-statement-options-1
    := alt-reel-unit alt-removal-no-rewind?)

(defrule close-statement-options-2
    := "WITH"? alt-no-rewind-lock)

(defrule close-statement-options
    := close-statement-options-1
    := close-statement-options-2)

(defrule close-statement-file-name
    := file-name close-statement-options?)

(defrule compute-statement
    := "COMPUTE" cobword-rounded+ equal arithmetic-expression on-size-error-statement-list? not-on-size-error-statement-list? "END-COMPUTE"?
    :reduce (list 'compute cobword-rounded arithmetic-expression :on-size-error on-size-error-statement-list
		  :not-on-size-error not-on-size-error-statement-list))

(defrule equal
    := "="
    := "EQUAL")

(defrule continue-statement
    := "CONTINUE")

(defrule delete-statement
    := "DELETE" file-name "RECORD"? invalid-key-statement-list? not-invalid-key-statement-list? "END-DELETE"?
    :reduce (list 'delete file-name :invalid invalid-key-statement-list :not-invalid not-invalid-key-statement-list))

(defrule display-statement
    := "DISPLAY" id-or-lit+ upon-environment-name? with-no-advancing?
    :reduce (list 'display (cons 'list id-or-lit) :upon upon-environment-name :advance (not with-no-advancing))
    := "DISPLAY" id-or-lit "AT" screen-coordinates
    :reduce (apply #'list 'display-at id-or-lit screen-coordinates))

(defrule screen-coordinates
    := integer
    :reduce (multiple-value-list (truncate integer 100)))

(defrule upon-environment-name
    := "UPON" cobol-identifier)

(defrule with-no-advancing
    := "WITH"? "NO" "ADVANCING")

(defrule divide-statement
    := "DIVIDE" id-or-lit "INTO" id-or-lit "GIVING" variable-identifier "ROUNDED"? "REMAINDER" variable-identifier on-size-error-statement-list? not-on-size-error-statement-list? "END-DIVIDE"?
    := "DIVIDE" id-or-lit "BY" id-or-lit "GIVING" variable-identifier "ROUNDED"? "REMAINDER" variable-identifier on-size-error-statement-list? not-on-size-error-statement-list? "END-DIVIDE"?
    := "DIVIDE" id-or-lit "INTO" id-or-lit "GIVING" cobword-rounded+ on-size-error-statement-list? not-on-size-error-statement-list? "END-DIVIDE"?
    := "DIVIDE" id-or-lit "BY" id-or-lit "GIVING" cobword-rounded+ on-size-error-statement-list? not-on-size-error-statement-list? "END-DIVIDE"?
    := "DIVIDE" id-or-lit "INTO" cobword-rounded+ on-size-error-statement-list? not-on-size-error-statement-list? "END-DIVIDE"?)

(defrule entry-statement
    := "ENTRY" literal using-phrase?)

(defrule evaluate-statement
    := "EVALUATE" evaluate-condition also-phrase* when-phrases+ when-other-phrase? "END-EVALUATE"?)

(defrule evaluate-condition
    := condition
    := "TRUE"
    := "FALSE")

(defrule also-phrase
    := "ALSO" evaluate-condition)

(defrule when-phrase-also-phrase
    := "ALSO" evaluate-phrase)

(defrule when-phrase
    := "WHEN" evaluate-phrase when-phrase-also-phrase*)

(defrule when-phrases
    := when-phrase+ statement-list)

(defrule when-other-phrase
    := "WHEN" "OTHER" statement-list)

(defrule evaluate-phrase
    := "ANY"
    := condition
    := "TRUE"
    := "FALSE"
    := evaluate-phrase-1)

(defrule evaluate-phrase-1
    := "NOT"? arithmetic-expression through-arithmetic-expression?)

(defrule through-arithmetic-expression
    := through arithmetic-expression)

(defrule exit-statement
    := "EXIT"
    :reduce '(exit-paragraph))

(defrule exit-program-statement
    := "EXIT" "PROGRAM"
    :reduce '(exit-program))

(defrule goback-statement
    := "GOBACK"
    :reduce '(go-back))

(defrule go-to-statement
    := "GO" "TO"? procedure-name+ "DEPENDING" "ON"? variable-identifier
    :reduce (list 'goto-depending variable-identifier procedure-name)
    := "GO" "TO"? procedure-name
    :reduce (list 'goto procedure-name))

(defrule if-phrase
    := "IF" condition "THEN"? alt-statement-list-next-sentence "ELSE" alt-statement-list-next-sentence
    :reduce (list 'if condition
		  (if (cdr alt-statement-list-next-sentence)
		      (cons 'progn alt-statement-list-next-sentence)
		      (car alt-statement-list-next-sentence))
		  (if (cdr alt-statement-list-next-sentence2)
		      (cons 'progn alt-statement-list-next-sentence2)
		      (car alt-statement-list-next-sentence2)))
    := "IF" condition "THEN"? alt-statement-list-next-sentence
    :reduce (append (list 'when condition) alt-statement-list-next-sentence))

(defrule if-statement
    := if-phrase "END-IF"?
    :reduce $1)

(defrule initialize-statement
    := "INITIALIZE" variable-identifier+ initialize-replacing-phrase?)

(defrule initialize-replacing-type
    := "ALPHABETIC"
    := "ALPHANUMERIC"
    := "NUMERIC"
    := "ALPHANUMERIC-EDITED"
    := "NUMERIC-EDITED"
    := "DBCS"
    := "EGCS")

(defrule initialize-replacing-argument
    := initialize-replacing-type "DATA"? "BY" id-or-lit)

(defrule initialize-replacing-phrase
    := "REPLACING" initialize-replacing-argument+)

(defrule inspect-statement
    := inspect-statement-1
    := inspect-statement-2
    := inspect-statement-3
    := inspect-statement-4)

(defrule inspect-statement-1
    := "INSPECT" variable-identifier "TALLYING" tallying-argument+)

(defrule inspect-statement-2
    := "INSPECT" variable-identifier "CONVERTING" id-or-lit "TO" id-or-lit before-after-phrase*)

(defrule inspect-statement-3
    := "INSPECT" variable-identifier "TALLYING" tallying-argument+ "REPLACING" inspect-replacing-phrase+)

(defrule tallying-for-id-or-lit
    := id-or-lit before-after-phrase*)

(defrule alt-all-leading
    := "ALL"
    := "LEADING")

(defrule tallying-for-argument-1
    := "CHARACTERS" before-after-phrase*)

(defrule tallying-for-argument-2
    := alt-all-leading tallying-for-id-or-lit+)

(defrule tallying-for-argument
    := tallying-for-argument-1
    := tallying-for-argument-2)

(defrule tallying-argument
    := variable-identifier "FOR" tallying-for-argument+)

(defrule inspect-statement-4
    := "INSPECT" variable-identifier "REPLACING" inspect-replacing-phrase+)

(defrule inspect-replacing-argument
    := inspect-by-argument "BY" inspect-by-argument before-after-phrase*)

(defrule alt-all-leading-first
    := "ALL"
    := "LEADING"
    := "FIRST")

(defrule inspect-replacing-phrase-1
    := "CHARACTERS" "BY" id-or-lit before-after-phrase*)

(defrule inspect-replacing-phrase-2
    := alt-all-leading-first inspect-replacing-argument+)

(defrule inspect-replacing-phrase
    := inspect-replacing-phrase-1
    := inspect-replacing-phrase-2)

(defrule before-after-phrase
    := alt-before-after "INITIAL"? id-or-lit)

(defrule merge-statement
    := "MERGE" file-name on-key-phrase+ collating-sequence? "USING" file-name file-name+ merge-statement-tail)

(defrule on-key-phrase
    := "ON"? alt-ascending-descending "KEY"? qualified-data-name+)

(defrule merge-statement-tail
    := output-procedure
    := giving-file-names)

(defrule move-statement
    := "MOVE" id-or-lit "TO" variable-identifier+
    :reduce (apply #'list 'move id-or-lit variable-identifier)
    := "MOVE" corresponding variable-identifier "TO" variable-identifier+
    :reduce (apply #'list 'move-corresponding variable-identifier variable-identifier2))

(defrule multiply-statement
    := "MULTIPLY" id-or-lit "BY" cobword-rounded+ on-size-error-statement-list? not-on-size-error-statement-list? "END-MULTIPLY"?
    :reduce (list 'multiply id-or-lit cobword-rounded :on-size-error on-size-error-statement-list
		  :not-on-size-error not-on-size-error-statement-list)
    := "MULTIPLY" id-or-lit "BY" id-or-lit "GIVING" cobword-rounded+ on-size-error-statement-list? not-on-size-error-statement-list? "END-MULTIPLY"?
    :reduce (list 'multiply id-or-lit id-or-lit2 :giving cobword-rounded
		  :on-size-error on-size-error-statement-list
		  :not-on-size-error not-on-size-error-statement-list))

(defrule open-statement
    := "OPEN" open-statement-phrase+
    :reduce (list 'open open-statement-phrase))

(defrule alt-reversed-with-no-rewind
    := "REVERSED"
    := with-no-rewind)

(defrule open-statement-input-file-name
    := file-name alt-reversed-with-no-rewind?)

(defrule with-no-rewind
    := "WITH"? "NO" "REWIND")

(defrule open-statement-output-file-name
    := file-name with-no-rewind?)

(defrule open-statement-input
    := "INPUT" open-statement-input-file-name+)

(defrule open-statement-output
    := "OUTPUT" open-statement-output-file-name+)

(defrule open-statement-i-o
    := "I-O" file-name+)

(defrule open-statement-extend
    := "EXTEND" file-name+)

(defrule open-statement-phrase
    := open-statement-input
    := open-statement-output
    := open-statement-i-o
    := open-statement-extend)

(defrule perform-statement
    := "PERFORM" procedure-name through-procedure-name? perform-until-phrase
    :reduce `(perform-until ,procedure-name ,through-procedure-name ,perform-until-phrase)
    := "PERFORM" procedure-name through-procedure-name? perform-varying-phrase perform-after-phrase*
    :reduce `(perform-varying ,perform-varying-phrase ,procedure-name ,through-procedure-name ,perform-after-phrase)
    := "PERFORM" procedure-name through-procedure-name? cobword-int "TIMES"
    :reduce `(perform-times ,cobword-int ,procedure-name ,through-procedure-name)
    := "PERFORM" procedure-name through-procedure-name?
    :reduce (append (list 'perform procedure-name) through-procedure-name))

(defrule perform-varying-phrase
    := with-test? "VARYING" variable-identifier "FROM" id-or-lit "BY" id-or-lit "UNTIL" condition)

(defrule perform-after-phrase
    := "AFTER" variable-identifier "FROM" id-or-lit "BY" id-or-lit "UNTIL" condition)

(defrule perform-until-phrase
    := with-test? "UNTIL" condition)

(defrule with-test
    := "WITH"? "TEST" alt-before-after
    :reduce alt-before-after)

(defrule read-statement
    := "READ" file-name "NEXT"? "RECORD"? into-identifier? key-is-qualified-data-name? invalid-key-statement-list? not-invalid-key-statement-list? at-end-statement-list? not-at-end-statement-list? "END-READ"?)

(defrule key-is-qualified-data-name
    := "KEY" "IS"? qualified-data-name)

(defrule release-statement
    := "RELEASE" record-name from-identifier?)

(defrule return-statement
    := "RETURN" file-name "RECORD"? into-identifier? "AT"? "END" statement-list not-at-end-statement-list? "END-RETURN"?)

(defrule into-identifier
    := "INTO" variable-identifier)

(defrule not-at-end-statement-list
    := "NOT" "AT"? "END" statement-list)

(defrule rewrite-statement
    := "REWRITE" record-name from-identifier? invalid-key-statement-list? not-invalid-key-statement-list? "END-REWRITE"?)

(defrule search-statement
    := search-statement-1
    := search-statement-2)

(defrule search-statement-1
    := "SEARCH" cobol-identifier varying-identifier? at-end-statement-list? when-condition-stats+ "END-SEARCH"?)

(defrule varying-identifier
    := "VARYING" variable-identifier)

(defrule when-condition-stats
    := "WHEN" condition alt-statement-list-next-sentence)

(defrule search-statement-2
    := "SEARCH" "ALL" variable-identifier at-end-statement-list? "WHEN" search-statement-condition search-statement-condition-tail* alt-statement-list-next-sentence "END-SEARCH"?)

(defrule at-end-statement-list
    := "AT"? "END" statement-list)

(defrule search-statement-equal-expression
    := variable-identifier "IS"? equal-to arithmetic-expression
    :reduce (list '= variable-identifier arithmetic-expression))

(defrule search-statement-condition
    := search-statement-equal-expression
    := condition-name-reference)

(defrule search-statement-condition-tail
    := "AND" search-statement-condition)

(defrule alt-statement-list-next-sentence
    := statement+
    := next-sentence
    :reduce :next-sentence)

(defrule set-statement
    := "SET" set-statement-phrase+)

(defrule sort-statement
    := "SORT" file-name on-key-is-phrase+ with-duplicates-in-order? collating-sequence? sort-statement-in sort-statement-out)

(defrule key-is
    := "KEY" "IS"?)

(defrule alt-ascending-descending
    := "ASCENDING"
    := "DESCENDING")

(defrule on-key-is-phrase
    := "ON"? alt-ascending-descending key-is? qualified-data-name+)

(defrule with-duplicates-in-order
    := "WITH"? "DUPLICATES" "IN"? "ORDER"?)

(defrule collating-sequence
    := "COLLATING"? "SEQUENCE" "IS"? alphabet-name)

(defrule through
    := "THROUGH"
    := "THRU")

(defrule through-procedure-name
    := through procedure-name
    :reduce procedure-name)

(defrule using-file-names
    := "USING" file-name+)

(defrule input-procedure
    := "INPUT" "PROCEDURE" "IS"? procedure-name through-procedure-name?)

(defrule giving-file-names
    := "GIVING" file-name+)

(defrule output-procedure
    := "OUTPUT" "PROCEDURE" "IS"? procedure-name through-procedure-name?)

(defrule sort-statement-in
    := using-file-names
    := input-procedure)

(defrule sort-statement-out
    := giving-file-names
    := output-procedure)

(defrule start-statement
    := "START" file-name key-is-rel-op-qualified-data-name? invalid-key-statement-list? not-invalid-key-statement-list? "END-START"?)

(defrule rel-op
    := equal-to
    :reduce '=
    := greater-than
    :reduce '>
    := greater-equal
    :reduce '>=)

(defrule key-is-rel-op-qualified-data-name
    := "KEY" "IS"? rel-op qualified-data-name
    :reduce (list rel-op qualified-data-name))

(defrule stop-statement
    := "STOP" alt-run-literal
    :reduce '(stop))

(defrule alt-run-literal
    := "RUN"
    := literal)

(defrule string-statement
    := "STRING" delimited-by-phrase+ "INTO" variable-identifier with-pointer-identifier? on-overflow-statement-list? not-on-overflow-statement-list? "END-STRING"?
    :reduce (list 'string-concat delimited-by-phrase variable-identifier :with-pointer with-pointer-identifier :on-overflow on-overflow-statement-list :not-on-overflow not-on-overflow-statement-list))

(defrule id-or-lit-size
    := literal
    := variable-identifier
    := "SIZE")

(defrule delimited-by-phrase
    := id-or-lit+ "DELIMITED" "BY"? id-or-lit-size
    :reduce (list id-or-lit id-or-lit-size))

(defrule subtract-statement
    := "SUBTRACT" id-or-lit+ "FROM" id-or-lit "GIVING" cobword-rounded+ on-size-error-statement-list? not-on-size-error-statement-list? "END-SUBTRACT"?
    :reduce (list 'subtract-giving id-or-lit id-or-lit2 cobword-rounded
		  :on-size-error on-size-error-statement-list
		  :not-on-size-error not-on-size-error-statement-list)
    := "SUBTRACT" id-or-lit+ "FROM" cobword-rounded+ on-size-error-statement-list? not-on-size-error-statement-list? "END-SUBTRACT"?
    :reduce (list 'subtract id-or-lit cobword-rounded
		  :on-size-error on-size-error-statement-list
		  :not-on-size-error not-on-size-error-statement-list)
    := "SUBTRACT" corresponding variable-identifier "FROM" variable-identifier "ROUNDED"? on-size-error-statement-list? not-on-size-error-statement-list? "END-SUBTRACT"?
    :reduce (list 'subtract-corr variable-identifier variable-identifier
		  :rounded (and $5 t)
		  :on-size-error on-size-error-statement-list
		  :not-on-size-error not-on-size-error-statement-list))

(defrule cobword-rounded
    := variable-identifier "ROUNDED"?
    :reduce (list variable-identifier (and $2 t)))

(defrule on-size-error-statement-list
    := "ON"? "SIZE" "ERROR" statement-list
    :reduce statement-list)

(defrule not-on-size-error-statement-list
    := "NOT" "ON"? "SIZE" "ERROR" statement-list
    :reduce statement-list)

(defrule corresponding
    := "CORRESPONDING"
    := "CORR")

(defrule unstring-statement
    := "UNSTRING" variable-identifier delimited-by-all-phrase? "INTO" unstring-statement-dst+ with-pointer-identifier? tallying-in-identifier? on-overflow-statement-list? not-on-overflow-statement-list? "END-UNSTRING"?
    :reduce (list 'unstring variable-identifier unstring-statement-dst
		  :delimited-by-all delimited-by-all-phrase
		  :with-pointer with-pointer-identifier
		  :tallying tallying-in-identifier
		  :on-overflow on-overflow-statement-list
		  :not-on-overflow not-on-overflow-statement-list))

(defrule id-or-lit
    := literal
    := variable-identifier)

(defrule or-all-id-or-lit
    := "OR" "ALL"? id-or-lit)

(defrule delimited-by-all-phrase
    := "DELIMITED" "BY"? "ALL"? id-or-lit or-all-id-or-lit*)

(defrule delimiter-in-identifier
    := "DELIMITER" "IN"? variable-identifier)

(defrule count-in-identifier
    := "COUNT" "IN"? variable-identifier)

(defrule unstring-statement-dst
    := variable-identifier delimiter-in-identifier? count-in-identifier?)

(defrule with-pointer-identifier
    := "WITH"? "POINTER" variable-identifier)

(defrule tallying-in-identifier
    := "TALLYING" "IN"? variable-identifier)

(defrule on-overflow-statement-list
    := "ON"? "OVERFLOW" statement-list)

(defrule not-on-overflow-statement-list
    := "NOT" "ON"? "OVERFLOW" statement-list)

(defrule write-statement
    := "WRITE" record-name from-identifier? advancing-phrase? write-exceptions "END-WRITE"?)

(defrule lines
    := "LINE"
    := "LINES")

(defrule cobword-int
    := cobol-identifier
    := integer)

(defrule nr-lines-phrase
    := cobword-int lines?)

(defrule page-phrase
    := nr-lines-phrase
    := "PAGE")

(defrule alt-before-after
    := "BEFORE"
    := "AFTER")

(defrule advancing-phrase
    := alt-before-after "ADVANCING"? page-phrase)

(defrule from-identifier
    := "FROM" variable-identifier)

(defrule invalid-key-statement-list
    := "INVALID" "KEY"? statement-list
    :reduce statement-list)

(defrule not-invalid-key-statement-list
    := "NOT" "INVALID" "KEY"? statement-list
    :reduce statement-list)

(defrule end-of-page
    := "END-OF-PAGE"
    := "EOP")

(defrule at-end-of-page-statement-list
    := "AT"? end-of-page statement-list
    :reduce statement-list)

(defrule not-at-end-of-page-statement-list
    := "NOT" "AT"? end-of-page statement-list
    :reduce statement-list)

;; This is left in the grammar but is not used.  COPYs are handled by
;; the lexical scanner.
(defrule copy-statement
    := "COPY" alt-text-name-literal in-library? "SUPPRESS"? copy-statement-replacing-phrase?)

(defrule in
    := "OF"
    := "IN")

(defrule alt-library-name-literal
    := library-name
    := literal)

(defrule in-library
    := in alt-library-name-literal)

(defrule copy-statement-by-phrase
    := copy-operand "BY" copy-operand)

(defrule copy-statement-replacing-phrase
    := "REPLACING" copy-statement-by-phrase+)

(defrule alt-text-name-literal
    := text-name
    := literal)

(defrule copy-operand
    := cobol-identifier
    := literal)

(defrule use-statement
    := use-statement-1
    := use-statement-2
    := use-statement-3)

(defrule use-statement-1
    := "USE" "GLOBAL"? "AFTER" "STANDARD"? alt-exception-error "PROCEDURE" "ON"? alt-file-names-i-o)

(defrule alt-exception-error
    := "EXCEPTION"
    := "ERROR")

(defrule use-statement-2
    := "USE" "GLOBAL"? "AFTER" "STANDARD"? alt-beginning-ending? alt-file-reel-unit? "LABEL" "PROCEDURE" "ON"? alt-file-names-i-o)

(defrule alt-beginning-ending
    := "BEGINNING"
    := "ENDING")

(defrule alt-file-reel-unit
    := "FILE"
    := "REEL"
    := "UNIT")

(defrule file-names
    := file-name+)

(defrule alt-file-names-i-o
    := file-names
    := "INPUT"
    := "OUTPUT"
    := "I-O"
    := "EXTEND")

(defrule use-statement-3
    := "USE" "FOR"? "DEBUGGING" "ON"? alt-procedures-all-procedures)

(defrule procedure-names
    := procedure-name+)

(defrule alt-procedures-all-procedures
    := procedure-names
    := all-procedures)

(defrule condition
    := combinable-condition
    := combinable-condition "AND" condition
    :reduce `(and ,combinable-condition ,condition)
    := combinable-condition "OR" condition
    :reduce `(or ,combinable-condition ,condition)
    := combinable-condition "AND" id-or-lit
    :reduce `(and ,combinable-condition (,(car combinable-condition) ,(cadr combinable-condition) ,id-or-lit))
    := combinable-condition "OR" id-or-lit
    :reduce `(or ,combinable-condition (,(car combinable-condition) ,(cadr combinable-condition) ,id-or-lit)))

(defrule combinable-condition
    := "NOT"? simple-condition
    :reduce (if $1
		(list 'not simple-condition)
		simple-condition))

(defrule simple-condition
    := class-condition
    := relation-condition
    := sign-condition
    := "(" condition ")"
    ;; not sure if it's necessary -wcp15/7/03.
    ;; := arithmetic-expression
    )

(defrule class-condition
    := variable-identifier "IS"? "NOT"? class-type
    :reduce (if $3
		(list 'not (list 'type-of variable-identifier (make-keyword class-type)))
		(list 'type-of variable-identifier (make-keyword class-type))))

(defrule class-type
    := "NUMERIC"
    := "ALPHABETIC"
    := "ALPHABETIC-LOWER"
    := "ALPHABETIC-UPPER"
    := "DBCS")

(defun unfold-subrelations (main-relation subs)
  (destructuring-bind (main-operator main-variable other-variable) main-relation
    (declare (ignore other-variable))
    (labels ((unfold (subs)
	       (if (null subs)
		   main-relation
		   (destructuring-bind (connection operator variable) (car subs)
		     (list connection
			   (list (or operator main-operator) main-variable variable)
			   (unfold (cdr subs)))))))
      (unfold subs))))

(defrule relation-condition
    ;; This is too complex
    ;; := arithmetic-expression relational-operator simple-condition
    := id-or-lit relational-operator id-or-lit subordinate-relation*
    :reduce (unfold-subrelations (list relational-operator id-or-lit id-or-lit2) subordinate-relation))

(defrule or-and
    := "OR" :reduce 'or
    := "AND" :reduce 'and)

(defrule subordinate-relation
    := or-and relational-operator? id-or-lit
    :reduce (list or-and relational-operator id-or-lit))

(defrule relational-operator
    := "IS"? relational-operator-type
    :reduce relational-operator-type)

(defrule less-than
    := "LESS" "THAN"?
    := "<")

(defrule greater-equal
    := "GREATER" "THAN"? "OR" "EQUAL" "TO"?
    := ">="
    := ">" "="
    := "NOT" "<"
    := "NOT" "LESS" "THAN"?)

(defrule less-equal
    := "LESS" "THAN"? "OR" "EQUAL" "TO"?
    := "<="
    := "<" "="
    := "NOT" ">"
    := "NOT" "GREATER" "THAN"?)

(defrule greater-than
    := "GREATER" "THAN"?
    := ">")

(defrule equal-to
    := "EQUAL" "TO"?
    := "=")

(defrule relational-operator-type
    := greater-equal
    :reduce 'cob>=
    := less-equal
    :reduce 'cob<=
    := greater-than
    :reduce 'cob>
    := less-than
    :reduce 'cob<
    := equal-to
    :reduce 'cob=
    := "NOT" equal-to
    :reduce 'cob-not=)

(defrule sign-condition
    := arithmetic-expression "IS"? "NOT"? sign-type
    :reduce (if $3
		`(not (,sign-type ,arithmetic-expression))
		`(,sign-type ,arithmetic-expression)))

(defrule sign-type
    := "POSITIVE" :reduce '>
    := "NEGATIVE" :reduce '<
    := "ZERO" :reduce '=
    := "ZEROES" :reduce '=
    := "ZEROS" :reduce '=)

(defrule procedure-name
    := paragraph-or-section-name in-section-name
    :reduce (list paragraph-or-section-name in-section-name)
    := paragraph-or-section-name
    :reduce paragraph-or-section-name)

(defrule in-section-name
    := in cobol-identifier
    :reduce cobol-identifier)

(defrule variable-identifier
    := qualified-data-name subscript-parentheses* ;; reference-modification?
    :reduce (if subscript-parentheses
		(list :aref qualified-data-name subscript-parentheses)
		qualified-data-name))

(defrule reference-modification
    := "(" leftmost-character-position ":" length? ")"
    :reduce (if length
		(list :range leftmost-character-position length)
		leftmost-character-position))

(defrule condition-name-reference
    := condition-name in-data-or-file-or-mnemonic-name* subscript-parentheses*)

(defrule in-data-or-file-or-mnemonic-name
    := in data-or-file-or-mnemonic-name)

(defrule subscript-parentheses
    := "(" subscript ")")

(defrule subscript
    := subscript-expression+)

(defrule plus-minus-integer
    := plus-or-minus integer)

(defrule subscript-expression-ambiguous
    := qualified-data-name plus-minus-integer?)

(defrule subscript-expression
    := literal
    := subscript-expression-ambiguous)

(defrule qualified-data-name
    := data-name in-data-or-file-name*
    :reduce (if in-data-or-file-name
		(list data-name in-data-or-file-name) ; incomplete -wcp15/7/03.
		data-name)
    := "ADDRESS" "OF" data-name
    :reduce (list 'address-of data-name)
    := "LENGTH" "OF" cobol-identifier
    :reduce (list 'length-of cobol-identifier))

(defrule in-data-or-file-name
    := in data-or-file-name)

(defrule leftmost-character-position
    := arithmetic-expression)

(defrule length
    := arithmetic-expression)

(defrule arithmetic-expression
    := times-div
    := times-div "+" arithmetic-expression
    :reduce `(+ ,times-div ,arithmetic-expression)
    := times-div "-" arithmetic-expression
    :reduce `(- ,times-div ,arithmetic-expression))

(defrule times-div
    := power
    := power "*" times-div
    :reduce `(* ,power ,times-div)
    := power "/" times-div
    :reduce `(/ ,power ,times-div))

(defrule power
    := plus-or-minus? basis
    := plus-or-minus? basis "**" power
    :reduce (if plus-or-minus
		`(plus-or-minus (expt basis basis2))
		`(expt basis basis2)))

(defrule plus-or-minus
    := "+"
    :reduce '+
    := "-"
    :reduce '-)

;; (defrule power-tail
;;     := "**" basis)

(defrule basis
    := literal
    := variable-identifier
    := "(" arithmetic-expression ")")

(defrule alphabet-name
    := cobol-identifier)

(defrule condition-name
    := cobol-identifier)

(defrule data-name
    := cobol-identifier)

(defrule cobol-identifier
    := identifier
    :reduce (intern (string-upcase identifier)))

(defrule file-name
    := cobol-identifier)

(defrule data-or-file-name
    := cobol-identifier)

(defrule index-name
    := cobol-identifier)

(defrule mnemonic-name
    := cobol-identifier)

(defrule data-or-file-or-mnemonic-name
    := cobol-identifier)

(defrule record-name
    := qualified-data-name)

(defrule symbolic-character
    := cobol-identifier)

(defrule library-name
    := cobol-identifier)

(defrule program-name
    := cobol-identifier
    := string)

(defrule text-name
    := cobol-identifier)

(defrule paragraph-or-section-name
    := cobol-identifier
    := integer)

(defrule computer-name
    := identifier)

(defrule environment-name
    := cobol-identifier)

(defrule assignment-name
    := cobol-identifier)

(defrule figurative-constant
    := figurative-constant-simple
    := figurative-constant-all)

(defrule figurative-constant-all
    := "ALL" literal)

(defrule literal
    := string
    := float
    := integer
    := figurative-constant)

)					; defun populate-grammar
