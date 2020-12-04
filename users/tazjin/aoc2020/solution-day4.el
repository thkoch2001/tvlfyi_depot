;; Advent of Code 2020 - Day 4

(require 'cl-lib)
(require 's)
(require 'dash)
(require 'f)

(cl-defstruct day4/passport
  byr ;; Birth Year
  iyr ;; Issue Year
  eyr ;; Expiration Year
  hgt ;; Height
  hcl ;; Hair Color
  ecl ;; Eye Color
  pid ;; Passport ID
  cid ;; Country ID
  )

(defun day4/parse-passport (input)
  (let* ((pairs (s-split " " (s-replace "\n" " " input) t))
         (slots
          (-map
           (lambda (pair)
             (pcase-let ((`(,key ,value) (s-split ":" (s-trim pair))))
               (list (intern (format ":%s" key)) value)))
           pairs)))
    (apply #'make-day4/passport (-flatten slots))))

(defun day4/parse-passports (input)
  (-map #'day4/parse-passport (s-split "\n\n" input t)))

(setq day4/input (day4/parse-passports (f-read "/tmp/aoc/day4.txt")))

;; Puzzle 1

(defun day4/validate (passport)
  "Check that all fields except CID are present."
  (cl-check-type passport day4/passport)
  (and (day4/passport-byr passport)
       (day4/passport-iyr passport)
       (day4/passport-eyr passport)
       (day4/passport-hgt passport)
       (day4/passport-hcl passport)
       (day4/passport-ecl passport)
       (day4/passport-pid passport)))

(message "Solution to day4/1: %s" (cl-loop for passport being the elements of day4/input
                                           count (day4/validate passport)))

;; Puzzle 2

(defun day4/year-bound (min max value)
  (and
   (s-matches? (rx (= 4 digit)) value)
   (<= min (string-to-number value) max)))

(defun day4/check-unit (unit min max value)
  (and
   (string-match (rx (group (+? digit)) (literal unit)) value)
   (<= min (string-to-number (match-string 1 value)) max)))

(defun day4/properly-validate (passport)
  "Opting for readable rather than clever here."
  (and
   (day4/validate passport)

   ;; byr (Birth Year) - four digits; at least 1920 and at most 2002.
   (day4/year-bound 1920 2002 (day4/passport-byr passport))

   ;; iyr (Issue Year) - four digits; at least 2010 and at most 2020.
   (day4/year-bound 2010 2020 (day4/passport-iyr passport))

   ;; eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
   (day4/year-bound 2020 2030 (day4/passport-eyr passport))

   ;; hgt (Height) - a number followed by either cm or in:
   ;; If cm, the number must be at least 150 and at most 193.
   ;; If in, the number must be at least 59 and at most 76.
   (or (day4/check-unit "cm" 150 193 (day4/passport-hgt passport))
       (day4/check-unit "in" 59 76 (day4/passport-hgt passport)))

   ;; hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
   (s-matches? (rx ?# (= 6 hex)) (day4/passport-hcl passport))

   ;; ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
   (-contains? '("amb" "blu" "brn" "gry" "grn" "hzl" "oth")
               (day4/passport-ecl passport))

   ;; pid (Passport ID) - a nine-digit number, including leading zeroes.
   (s-matches? (rx line-start (= 9 digit) line-end)
               (day4/passport-pid passport))

   ;; cid (Country ID) - ignored, missing or not.
   ))

(message "Solution to day4/2: %s"
         (cl-loop for passport being the elements of day4/input
                  count (day4/properly-validate passport)))
