;; Advent of Code 2020 - Day 5

(require 'cl-lib)
(require 'dash)
(require 'f)
(require 'ht)
(require 's)
(require 'seq)

(defvar day5/input
  (-filter (lambda (s) (not (seq-empty-p s)))
           (s-lines (f-read "/tmp/aoc/day5.txt"))))

(defun day5/lower (sequence)
  (seq-subseq sequence 0 (/ (length sequence) 2)))

(defun day5/upper (sequence)
  (seq-subseq sequence (/ (length sequence) 2)))

(defun day5/seat-id (column row)
  (+ column (* 8 row)))

(defun day5/find-seat (boarding-pass)
  (let ((rows (number-sequence 0 127))
        (columns (number-sequence 0 7)))
    (cl-loop for char across boarding-pass
             do (pcase char
                  (?F (setq rows (day5/lower rows)))
                  (?B (setq rows (day5/upper rows)))
                  (?R (setq columns (day5/upper columns)))
                  (?L (setq columns (day5/lower columns))))
             finally return (day5/seat-id (car columns) (car rows)))))

;; Puzzle 1

(message "Solution to day5/1: %s"
         (cl-loop for boarding-pass in day5/input
                  maximize (day5/find-seat boarding-pass)))

;; Puzzle 2

(defun day5/all-seats-in (row)
  (-map (lambda (column) (day5/seat-id column row))
        (number-sequence 0 7)))

(message "Solution to day5/2: %s"
         (let ((all-seats (ht-create)))
           (-each (-mapcat #'day5/all-seats-in (number-sequence 1 126))
             (lambda (seat) (ht-set all-seats seat nil)))

           (cl-loop for boarding-pass in day5/input
                    do (ht-remove all-seats (day5/find-seat boarding-pass))

                    ;; Remove seats that lack adjacent entries, those
                    ;; are missing on the plane.
                    finally return
                    (car
                     (-filter (lambda (seat)
                                (and (not (ht-contains? all-seats (- seat 1)))
                                     (not (ht-contains? all-seats (+ seat 1)))))
                              (ht-keys all-seats))))))
