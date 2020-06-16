;;; private/grfn/splitjoin.el -*- lexical-binding: t; -*-

(require 'dash)
(load! "utils")

;;;
;;; Vars
;;;

(defvar +splitjoin/split-callbacks '()
  "Alist mapping major mode symbol names to lists of split callbacks")

(defvar +splitjoin/join-callbacks '()
  "Alist mapping major mode symbol names to lists of join callbacks")



;;;
;;; Definition macros
;;;

(defmacro +splitjoin/defsplit (mode name &rest body)
  `(setf
    (alist-get ',name (alist-get ,mode +splitjoin/split-callbacks))
    (λ! () ,@body)))

(defmacro +splitjoin/defjoin (mode name &rest body)
  `(setf
    (alist-get ',name (alist-get ,mode +splitjoin/join-callbacks))
    (λ! () ,@body)))

;;;
;;; Commands
;;;

(defun +splitjoin/split ()
  (interactive)
  (when-let (callbacks (->> +splitjoin/split-callbacks
                            (alist-get major-mode)
                            (-map #'cdr)))
    (find-if #'funcall callbacks)))

(defun +splitjoin/join ()
  (interactive)
  (when-let (callbacks (->> +splitjoin/join-callbacks
                            (alist-get major-mode)
                            (-map #'cdr)))
    (find-if #'funcall callbacks)))


;;;
;;; Splits and joins
;;; TODO: this should probably go in a file-per-language
;;;

(+splitjoin/defjoin
 'elixir-mode
 join-do
 (let* ((function-pattern (rx (and (zero-or-more whitespace)
                                   "do"
                                   (zero-or-more whitespace)
                                   (optional (and "#" (zero-or-more anything)))
                                   eol)))
        (end-pattern (rx bol
                         (zero-or-more whitespace)
                         "end"
                         (zero-or-more whitespace)
                         eol))
        (else-pattern (rx bol
                         (zero-or-more whitespace)
                         "else"
                         (zero-or-more whitespace)
                         eol))
        (lineno     (line-number-at-pos))
        (line       (thing-at-point 'line t)))
   (when-let ((do-start-pos (string-match function-pattern line)))
     (cond
      ((string-match-p end-pattern (get-line (inc lineno)))
       (modify-then-indent
        (goto-line-char do-start-pos)
        (insert ",")
        (goto-char (line-end-position))
        (insert ": nil")
        (line-move 1)
        (delete-line))
       t)

      ((string-match-p end-pattern (get-line (+ 2 lineno)))
       (modify-then-indent
        (goto-line-char do-start-pos)
        (insert ",")
        (goto-char (line-end-position))
        (insert ":")
        (join-line t)
        (line-move 1)
        (delete-line))
       t)

      ((and (string-match-p else-pattern (get-line (+ 2 lineno)))
            (string-match-p end-pattern  (get-line (+ 4 lineno))))
       (modify-then-indent
        (goto-line-char do-start-pos)
        (insert ",")
        (goto-char (line-end-position))
        (insert ":")
        (join-line t)
        (goto-eol)
        (insert ",")
        (join-line t)
        (goto-eol)
        (insert ":")
        (join-line t)
        (line-move 1)
        (delete-line))
       t)))))

(comment
 (string-match (rx (and bol
                        "if "
                        (one-or-more anything)
                        ","
                        (zero-or-more whitespace)
                        "do:"
                        (one-or-more anything)
                        ","
                        (zero-or-more whitespace)
                        "else:"
                        (one-or-more anything)))
               "if 1, do: nil, else: nil")

 )

(+splitjoin/defsplit
 'elixir-mode
 split-do-with-optional-else
 (let* ((if-with-else-pattern (rx (and bol
                                       (one-or-more anything)
                                       ","
                                       (zero-or-more whitespace)
                                       "do:"
                                       (one-or-more anything)
                                       (optional
                                        ","
                                        (zero-or-more whitespace)
                                        "else:"
                                        (one-or-more anything)))))
        (current-line (get-line)))
   (when (string-match if-with-else-pattern current-line)
     (modify-then-indent
      (assert (goto-regex-on-line ",[[:space:]]*do:"))
      (delete-char 1)
      (assert (goto-regex-on-line ":"))
      (delete-char 1)
      (insert "\n")
      (when (goto-regex-on-line-r ",[[:space:]]*else:")
        (delete-char 1)
        (insert "\n")
        (assert (goto-regex-on-line ":"))
        (delete-char 1)
        (insert "\n"))
      (goto-eol)
      (insert "\nend"))
     t)))

(comment
 (+splitjoin/defsplit 'elixir-mode split-def
 (let ((function-pattern (rx (and ","
                                  (zero-or-more whitespace)
                                  "do:")))
       (line (thing-at-point 'line t)))
   (when-let (idx (string-match function-pattern line))
     (let ((beg (line-beginning-position))
           (orig-line-char (- (point) (line-beginning-position))))
       (save-mark-and-excursion
        (goto-line-char idx)
        (delete-char 1)
        (goto-line-char (string-match ":" (thing-at-point 'line t)))
        (delete-char 1)
        (insert "\n")
        (goto-eol)
        (insert "\n")
        (insert "end")
        (evil-indent beg (+ (line-end-position) 1))))
     (goto-line-char orig-line-char)
     t))))

(+splitjoin/defjoin
 'elixir-mode
 join-if-with-else
 (let* ((current-line (thing-at-point 'line)))))

(provide 'splitjoin)
