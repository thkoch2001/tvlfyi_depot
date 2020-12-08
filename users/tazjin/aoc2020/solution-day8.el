;; Advent of Code 2020 - Day

(require 'cl-lib)
(require 'dash)
(require 'f)
(require 's)

(setq day8/input
      (apply #'vector
             (-map (lambda (s)
                     (pcase-let ((`(,op ,val) (s-split " " s t)))
                       (cons (intern op) (string-to-number val))))
                   (s-lines (s-chomp (f-read "/tmp/aoc/day8.txt"))))))

(defun day8/step (code position acc)
  (let ((current (aref code position)))
    (aset code position nil)
    (pcase current
      ('() (cons 'final acc))
      (`(nop . ,val) (cons (+ position 1) acc))
      (`(acc . ,val) (cons (+ position 1) (+ acc val)))
      (`(jmp . ,val) (cons (+ position val) acc)))))

;; Puzzle 1

(message "Solution to day8/1: %s"
 (let ((code (copy-sequence day8/input))
       (position 0)
       (acc 0))
   (cl-loop for next = (day8/step code position acc)
            when (equal 'final (car next)) return (cdr next)
            do (setq position (car next))
            do (setq acc (cdr next)))))
