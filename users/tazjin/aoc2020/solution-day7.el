;; Advent of Code 2020 - Day 7

(require 'cl-lib)
(require 'dash)
(require 'f)
(require 's)
(require 'ht)

(defvar day7/input
  (s-lines (s-chomp (f-read "/tmp/aoc/day7.txt"))))

(defun day7/parse-bag (input)
  (string-match (rx line-start
                    (group (one-or-more (or letter space)))
                    "s contain "
                    (group (one-or-more anything))
                    "." line-end)
                input)
  (cons (match-string 1 input)
        (-map
         (lambda (content)
           (unless (equal content "no other bags")
             (progn
               (string-match
                (rx (group (one-or-more digit))
                    space
                    (group (one-or-more anything) "bag"))
                content)
               (cons (match-string 2 content)
                     (string-to-number (match-string 1 content))))))
         (s-split ", " (match-string 2 input)))))

(defun day7/id-or-next (table bag-type)
  (unless (ht-contains? table bag-type)
    (ht-set table bag-type (length (ht-keys table))))
  (ht-get table bag-type))

(defun day7/build-graph (input &optional flip)
  "Represent graph mappings directionally using an adjacency
  matrix, because that's probably easiest.

  By default an edge means 'contains', with optional argument
  FLIP edges are inverted and mean 'contained by'."

  (let ((bag-mapping (ht-create))
        (graph (let ((length (length input)))
                 (apply #'vector
                        (-map (lambda (_) (make-vector length 0)) input)))))
    (cl-loop for bag in (-map #'day7/parse-bag input)
             for bag-id = (day7/id-or-next bag-mapping (car bag))
             do (-each (-filter #'identity (cdr bag))
                  (pcase-lambda (`(,contained-type . ,count))
                    (let ((contained-id (day7/id-or-next bag-mapping contained-type)))
                      (if flip
                          (aset (aref graph contained-id) bag-id count)
                        (aset (aref graph bag-id) contained-id count))))))
    (cons bag-mapping graph)))

;; Puzzle 1

(defun day7/find-ancestors (visited graph start)
  (ht-set visited start t)
  (cl-loop for bag-count being the elements of (aref graph start)
           using (index bag-id)
           when (and (> bag-count 0)
                     (not (ht-contains? visited bag-id)))
           do (day7/find-ancestors visited graph bag-id)))

(message
 "Solution to day7/1: %s"
 (pcase-let* ((`(,mapping . ,graph) (day7/build-graph day7/input t))
              (shiny-gold-id (ht-get mapping "shiny gold bag"))
              (visited (ht-create)))
   (day7/find-ancestors visited graph shiny-gold-id)
   (- (length (ht-keys visited)) 1)))

;; Puzzle 2

(defun ht-find-by-value (table value)
  (ht-find (lambda (_key item-value) (equal item-value value)) table))

(defun day7/count-contained-bags (mapping graph start)
  (cl-loop for bag-count being the elements of (aref graph start)
           using (index bag-id)
           when (> bag-count 0)
           sum (+ bag-count
                  (* bag-count (day7/count-contained-bags mapping graph bag-id)))))

(message "Solution to day7/2: %s"
         (pcase-let* ((`(,mapping . ,graph) (day7/build-graph day7/input))
                      (shiny-gold-id (ht-get mapping "shiny gold bag")))
           (day7/count-contained-bags mapping graph shiny-gold-id)))
