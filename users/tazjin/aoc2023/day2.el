(require 'dash)
(require 's)
(require 'f)

(defvar aoc23-day2-example

  "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green")

;; part 1

(cl-defstruct aoc23d2-set red green blue)

(defun aoc23d2-parse-set (input)
  (let ((set (make-aoc23d2-set :red 0 :green 0 :blue 0))
        (colours (-map #'s-trim (s-split "," input))))
    (cl-loop for colour in colours
             do (pcase (s-split " " colour t)
                  (`(,num "red") (setf (aoc23d2-set-red set) (string-to-number num)))
                  (`(,num "green") (setf (aoc23d2-set-green set) (string-to-number num)))
                  (`(,num "blue") (setf (aoc23d2-set-blue set) (string-to-number num)))))
    set))

(cl-defstruct aoc23d2-game id sets)

(defun aoc23d2-parse-game (input)
  (pcase-let* ((`(,id-str ,sets-str) (s-split-up-to ":" input 1 t))
               (game-id (string-to-number (s-chop-left (length "Game ") id-str)))
               (sets (-map #'aoc23d2-parse-set (s-split ";" sets-str t))))
    (make-aoc23d2-game :id game-id :sets sets)))

(defun aoc23d2-game-possible-p (game r g b)
  (cl-every (lambda (set)
              (and (<= (aoc23d2-set-red set) r)
                   (<= (aoc23d2-set-green set) g)
                   (<= (aoc23d2-set-blue set) b)))
            (aoc23d2-game-sets game)))

(let ((input (f-read "~/Downloads/input.txt")))
  (-sum
   (-map #'aoc23d2-game-id
         (-filter (lambda (g) (aoc23d2-game-possible-p g 12 13 14))
                  (-map #'aoc23d2-parse-game (s-lines (s-trim input)))))))

;; part 2

(defun aoc23d2-game-min-cubes-power (game)
  (let ((r 0)
        (g 0)
        (b 0))
    (-each (aoc23d2-game-sets game)
      (lambda (set)
        (setq r (max r (aoc23d2-set-red set)))
        (setq g (max g (aoc23d2-set-green set)))
        (setq b (max b (aoc23d2-set-blue set)))))
    (* (max 1 r) (max 1 g) (max 1 b))))

(let ((input (f-read "~/Downloads/input.txt")))
  (-sum
   (-map #'aoc23d2-game-min-cubes-power
         (-map #'aoc23d2-parse-game (s-lines (s-trim input))))))
