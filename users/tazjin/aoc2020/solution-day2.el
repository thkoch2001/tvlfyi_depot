;; Advent of Code 2020 - Day 2

(require 'cl-lib)
(require 'f)
(require 'ht)
(require 's)
(require 'seq)

(defvar day2/input
  ;; This one was too large to inline.
  (s-lines (f-read "/tmp/aoc/day2.txt")))

(defun day2/count-letters (password)
  (let ((table (ht-create)))
    (cl-loop for char across password
             for current = (ht-get table char)
             do (ht-set table char
                        (if current (+ 1 current) 1)))
    table))

(defun day2/parse (input)
  (let* ((split (s-split " " input))
         (range (s-split "-" (car split))))
    (list (string-to-number (car range))
          (string-to-number (cadr range))
          (string-to-char (cadr split))
          (caddr split))))

(defun day2/count-with-validation (func)
  (length (-filter
           (lambda (password)
             (and (not (seq-empty-p password))
                  (apply func (day2/parse password))))
           day2/input)))

;; Puzzle 1

(defun day2/validate-oldjob (min max char password)
  (let ((count (ht-get (day2/count-letters password) char)))
    (when count
      (and (>= count min)
           (<= count max)))))

(message "Solution to day2/1: %s"
         (day2/count-with-validation #'day2/validate-oldjob))

;; Puzzle 2

(defun day2/validate-toboggan (pos1 pos2 char password)
  (xor (= char (aref password (- pos1 1)))
       (= char (aref password (- pos2 1)))))

(message "Solution to day2/2: %s"
         (day2/count-with-validation #'day2/validate-toboggan))
