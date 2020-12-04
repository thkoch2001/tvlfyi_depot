;; Advent of Code 2020 - Day 3

(require 'cl-lib)
(require 'dash)
(require 'f)
(require 's)
(require 'seq)

(setq day3/input
      (-filter (lambda (s) (not (seq-empty-p s)))
         (s-lines (f-read "/tmp/aoc/day3.txt"))))

(setq day3/input-width (length (elt day3/input 0)))
(setq day3/input-height (length day3/input))

(defun day3/thing-at-point (x y)
  "Pun intentional."
  (when (>= day3/input-height y)
    (let ((x-repeated (mod (- x 1) day3/input-width)))
      (elt (elt day3/input (- y 1)) x-repeated))))

(defun day3/slope (x-steps y-steps)
  "Produce the objects encountered through this slope until the
  bottom of the map."
  (cl-loop for x from 1 by x-steps
           for y from 1 to day3/input-height by y-steps
           collect (day3/thing-at-point x y)))

;; Puzzle 1

(defun day3/count-trees (x-steps y-steps)
  (cl-loop for thing being the elements of (day3/slope x-steps y-steps)
           count (= thing ?#)))

(message "Solution to day3/1: One encounters %s trees" (day3/count-trees 3 1))

;; Puzzle 2

(message "Solution to day3/2 %s" (* (day3/count-trees 1 1)
                                    (day3/count-trees 3 1)
                                    (day3/count-trees 5 1)
                                    (day3/count-trees 7 1)
                                    (day3/count-trees 1 2)))
