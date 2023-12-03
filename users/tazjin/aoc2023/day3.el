(defun aoc23d3-symbol-p (c)
  (not (or (= c ? )
           (and (>= c ?0)
                (<= c ?9)))))

(defun rectangle-for-bounds (bounds)
  (let* ((start (save-excursion
                     (goto-char (car bounds))
                     (let ((col (current-column)))
                       (forward-line -1)
                       (move-to-column (max 0 (1- col))))
                     (point)))
         (end (save-excursion
                (goto-char (cdr bounds))
                (let ((col (current-column)))
                  (forward-line 1)
                  (move-to-column (1+ col)))
                (point))))
    (list start end)))

(defun get-machine-part ()
  (interactive)
  (when-let* ((num-raw (number-at-point))
              (num (abs num-raw))
              ;; handles negative number edge case (bounds contain the `-')
              (bounds-raw (bounds-of-thing-at-point 'number))
              (bounds (if (< num-raw 0)
                          (cons (1- (car bounds-raw)) (cdr bounds-raw))
                        bounds-raw))
              (rectangle (rectangle-for-bounds bounds))
              (neighbours (apply #'concat
                                 (apply #'extract-rectangle rectangle))))
    (if (-any #'aoc23d3-symbol-p (string-to-list neighbours))
        (cons num rectangle)
      (cons nil rectangle))))


(defun find-machine-parts (input)
  (with-temp-buffer
    (insert input)
    (goto-char (point-min))
    (save-excursion
      (replace-string "." " "))

    (cl-loop while (forward-word)
             for result = (get-machine-part)
             when (car result) collect (car result))))


;; debugging

(defvar aoc23d3-example "467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..")

(defvar aoc23d3-example2 "12.......*..
+.........34
.......-12..
..78........
..*....60...
78..........
.......23...
....90*12...
............
2.2......12.
.*.........*
1.1.......56")

(defvar aoc23d3-example3 "243.
..*.
....")

(defun aoc23d3-debug (p)
  "Interactive debugger for the solution, can be bound to a key in
an input buffer. Dots should already have been replaced with
spaces."
  (interactive "P")
  (unless p
    (goto-char aoc23d3-last))
  (rectangle-mark-mode 1)
  (forward-word)
  (setq aoc23d3-last (point))
  (pcase (get-machine-part)
    (`(nil ,b ,e) (progn (set-mark b)
                          (goto-char e)
                          (set-face-attribute 'region nil :background "#FAA0A0")))
    (`(,num ,b ,e) (progn (set-mark b)
                          (goto-char e)
                          (set-face-attribute 'region nil :background "#d1ffbd")))
    (other (deactivate-mark))))

(cl-assert (= 4361 (-sum (find-machine-parts aoc23d3-example))) nil
           "example from website is working")

(cl-assert (= 413 (-sum (find-machine-parts aoc23d3-example2))) nil
           "example from subreddit is working")

(cl-assert (= 243 (-sum (find-machine-parts aoc23d3-example3))) nil
           "example from telegram is working")

;; day 1 (incomplete)

(-sum (find-machine-parts (s-trim (f-read "~/Downloads/input.txt"))))
