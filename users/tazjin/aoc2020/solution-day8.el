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
  (if (>= position (length code))
      (cons 'final acc)

    (let ((current (aref code position)))
      (aset code position :done)
      (pcase current
        (:done (cons 'loop acc))
        (`(nop . ,val) (cons (+ position 1) acc))
        (`(acc . ,val) (cons (+ position 1) (+ acc val)))
        (`(jmp . ,val) (cons (+ position val) acc))))))

;; Puzzle 1

(message "Solution to day8/1: %s"
         (let ((code (copy-sequence day8/input))
               (position 0)
               (acc 0))
           (cl-loop for next = (day8/step code position acc)
                    when (equal 'loop (car next)) return (cdr next)
                    do (setq position (car next))
                    do (setq acc (cdr next)))))

;; Puzzle 2

(defun day8/flip-at (code pos)
  (pcase (aref code pos)
    (`(nop . ,val) (aset code pos `(jmp . ,val)))
    (`(jmp . ,val) (aset code pos `(nop . ,val)))
    (other (error "Unexpected flip op: %s" other))))

(defun day8/try-flip (flip-at code position acc)
  (day8/flip-at code flip-at)
  (cl-loop for next = (day8/step code position acc)
           when (equal 'loop (car next)) return nil
           when (equal 'final (car next)) return (cdr next)
           do (setq position (car next))
           do (setq acc (cdr next))))

(message "Solution to day8/2: %s"
         (let ((flip-options (cl-loop for op being the elements of day8/input
                                      using (index idx)
                                      for opcode = (car op)
                                      when (or (equal 'nop opcode)
                                               (equal 'jmp opcode))
                                      collect idx)))
           (cl-loop for flip-at in flip-options
                    for result = (day8/try-flip flip-at (copy-sequence day8/input) 0 0)
                    when result return result)))
