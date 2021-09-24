(require 'ht)
(require 'cl-macs)

(cl-defstruct russian-word
  "Definition and metadata of a single Russian word."
  (word nil :type string)
  (translations :type list
                :documentation "List of lists of strings, each a set of translations.")

  (notes nil :type list ;; of string
         :documentation "free-form notes about this word")

  (roots nil :type list ;; of string
         :documentation "list of strings that correspond with roots (exact string match)"))

(defun russian--merge-words (word1 word2)
  "Merge two Russian word definitions together. If no second word
is provided, the first one is returned unaltered."
  (if (not word2) word1
    (assert (equal (russian-word-word word1)
                   (russian-word-word word2))
            "different words passed into merge function")
    (make-russian-word :word (russian-word-word word1)
                       :translations (-concat (russian-word-translations word1)
                                              (russian-word-translations word2))
                       :notes (-concat (russian-word-notes word1)
                                       (russian-word-notes word2))
                       :roots (-concat (russian-word-roots word1)
                                       (russian-word-roots word2)))))

(defvar russian-words (make-hash-table)
  "Table of all Russian words in the corpus.")

;; (defmacro define-words (&rest words)
;;   "Define the list of all available words. There may be more than
;; one entry for a word in some cases."
;;   (declare (indent defun)))

(provide 'russian-defs)
