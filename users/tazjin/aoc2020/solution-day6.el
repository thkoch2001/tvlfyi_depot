;; Advent of Code 2020 - Day 6

(require 'cl-lib)
(require 'dash)
(require 'f)
(require 'ht)
(require 's)

(defvar day6/input (s-split "\n\n" (f-read "/tmp/aoc/day6.txt") t)
  "Input, split into groups (with people in each group still distinct)")

;; Puzzle 1

(defun day6/count-answers (group-answers)
  "I suspect doing it this way will be useful in puzzle 2."
  (let ((table (ht-create)))
    (-each group-answers
      (lambda (answer)
        (cl-loop for char across answer
                 do (ht-set table char (+ 1 (or (ht-get table char)
                                                0))))))
    table))

(message "Solution to day6/1: %s"
         (cl-loop for group being the elements of day6/input
                  sum (length
                       (ht-keys
                        (day6/count-answers (s-lines group))))))

;; Puzzle 2

(defun day6/count-unanimous-answers (answers)
  (ht-reject (lambda (_key value) (not (= value (length answers))))
             (day6/count-answers answers)))

(message "Solution to day6/2: %s"
         (cl-loop for group being the elements of day6/input
                  sum (length
                       (ht-keys
                        (day6/count-unanimous-answers (s-split "\n" group t))))))
