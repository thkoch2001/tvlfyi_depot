(require 's)
(require 'f)

;; task 1

(defun digit-p (c)
  (and (> c ?0)
       (<= c ?9)))

(defun aocd1-sum-values (lines)
  (-sum
   (-map (lambda (line)
           (let ((digits (-filter #'digit-p (string-to-list line))))
             (string-to-number (string (-first-item digits) (-last-item digits)))))
         lines)))

(let ((lines (s-lines (s-trim (f-read "~/Downloads/input.txt")))))
  (aocd1-sum-values lines))

;; task 2

(defun replace-written-numbers (input)
  (with-temp-buffer
    (insert input)
    (let ((start 1))
      (while (< start (point-max))
        (format-replace-strings
         '(("oneight" . "18")
           ("twone" . "21")
           ("threeight" . "38")
           ("fiveight" . "58")
           ("sevenine" . "79")
           ("eightwo" . "82")
           ("eighthree" . "83")
           ("nineight" . "98"))
         nil start (min (+ 10 start) (point-max)))
        (format-replace-strings
         '(("one" . "1")
           ("two" . "2")
           ("three" . "3")
           ("four" . "4")
           ("five" . "5")
           ("six" . "6")
           ("seven" . "7")
           ("eight" . "8")
           ("nine" . "9"))
         nil start (min (+ 5 start) (point-max)))
        (setq start (1+ start))))
    (buffer-string)))

(let ((lines (s-lines (s-trim (f-read "~/Downloads/input.txt")))))
  (aocd1-sum-values (-map #'replace-written-numbers lines)))
