(require 'cl-macs)
(require 'ht)
(require 'seq)
(require 's)

;; Type definitions for Russian structures

(cl-defstruct russian-word
  "Definition and metadata of a single Russian word."
  (word nil :type string)
  (translations :type list
                :documentation "List of lists of strings, each a set of translations.")

  (notes nil :type list ;; of string
         :documentation "free-form notes about this word")

  (roots nil :type list ;; of string
         :documentation "list of strings that correspond with roots (exact string match)"))

(defun russian--merge-words (previous new)
  "Merge two Russian word definitions together. If no previous
  definition exists, only the new one will be returned."
  (if (not previous) new
    (assert (equal (russian-word-word previous)
                   (russian-word-word new))
            "different words passed into merge function")
    (make-russian-word :word (russian-word-word previous)
                       :translations (-concat (russian-word-translations previous)
                                              (russian-word-translations new))
                       :notes (-concat (russian-word-notes previous)
                                       (russian-word-notes new))
                       :roots (-concat (russian-word-roots previous)
                                       (russian-word-roots new)))))

;; Definitions for creating a data structure of all Russian words.

(defvar russian-words (make-hash-table)
  "Table of all Russian words in the corpus.")

(defun russian--define-word (word)
  "Define a single word in the corpus, optionally merging it with
  another entry."
  (let ((key (russian-word-word word)))
    (ht-set russian-words key (russian--merge-words
                               (ht-get russian-words key)
                               word))))

(defmacro define-russian-words (&rest words)
  "Define the list of all available words. There may be more than
  one entry for a word in some cases."
  (declare (indent defun))

  ;; Clear the table before proceeding with insertion
  (setq russian-words (make-hash-table))

  (seq-map
   (lambda (word)
     (russian--define-word (make-russian-word :word (car word)
                                              :translations (cadr word)
                                              :notes (caddr word)
                                              :roots (cadddr word))))
   words)

  '(message "Defined %s unique words." (ht-size russian-words)))

;; Helpers to train Russian words when Emacs is idling.

(defun russian--format-word (word)
  "Format a Russian word suitable for echo display."
  (apply #'s-concat
         (-flatten
          (list (russian-word-word word)
                " - "
                (s-join ", " (russian-word-translations word))
                (when-let ((roots (russian-word-roots word)))
                  (list " [" (s-join ", " roots) "]"))
                (when-let ((notes (russian-word-notes word)))
                  (list " (" (s-join "; " notes) ")"))))))

(defvar russian--last-word nil
  "Last randomly displayed Russian word")

(defun display-random-russian-word ()
  (interactive)
  (let* ((word (seq-random-elt (ht-values russian-words))))
    (while (ht-contains? russian--known-words (russian-word-word word))
      (setq word (seq-random-elt (ht-values russian-words))))
    (setq russian--last-word word)
    (message (russian--format-word word))))

(defvar russian--display-timer
  (run-with-idle-timer 5 t #'display-random-russian-word))

;; Ability to filter out known words

(defvar russian--known-words (make-hash-table)
  "Table of words that are already known.")

(defun persist-known-russian-words ()
  "Persist all known Russian words."
  (let ((file "/persist/tazjin/known-russian-words.el"))
    (with-temp-file file
      (insert (prin1-to-string russian--known-words)))))

(defun load-known-russian-words ()
  "Load all known Russian words."
  (let ((file "/persist/tazjin/known-russian-words.el"))
    (with-temp-buffer
      (insert-file-contents file)
      (setq russian--known-words (read (current-buffer))))))

(defun mark-last-russian-word-known ()
  "Mark the last Russian word that appeared as known."
  (interactive)
  (let ((word (russian-word-word russian--last-word)))
    (ht-set russian--known-words word t)
    (persist-known-russian-words)
    (message "Marked '%s' as known" word)))

(defun lookup-last-russian-word (in-eww)
  "Look up the last Russian word in Wiktionary"
  (interactive "P")
  (let ((url (concat "https://ru.wiktionary.org/wiki/" (russian-word-word russian--last-word))))
    (if in-eww (eww url)
      (browse-url url))))

(provide 'russian)
