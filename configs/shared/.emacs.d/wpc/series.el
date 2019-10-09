;;; series.el --- Hosting common series of numbers -*- lexical-binding: t -*-
;; Author: William Carroll <wpcarro@gmail.com>

;;; Commentary:
;; Encoding number series as I learn about them.
;;
;; These are the following series I'm interested in supporting:
;; - Fibonacci
;; - Catalan numbers
;; - Figurate number series
;;   - Triangular
;;   - Square
;;   - Pentagonal
;;   - Hexagonal
;;   - Lazy-caterer
;; - Magic square
;; - Look-and-say

;;; Code:

(require 'number)

(defun series/range (beg end)
  "Create a list of numbers from `BEG' to `END'.
This is an inclusive number range."
  (if (< end beg)
      (list/reverse
       (number-sequence end beg))
    (number-sequence beg end)))

(defun series/fibonacci-number (i)
  "Return the number in the fibonacci series at `I'."
  (cond
   ((= 0 i) 0)
   ((= 1 i) 1)
   (t (+ (series/fibonacci-number (- i 1))
         (series/fibonacci-number (- i 2))))))

(defun series/fibonacci (n)
  "Return the first `N' numbers of the fibonaccci series starting at zero."
  (if (= 0 n)
      '()
    (list/reverse
     (list/cons (series/fibonacci-number (number/dec n))
                (list/reverse
                 (series/fibonacci (number/dec n)))))))

;; TODO: Consider memoization.
(defun series/triangular-number (i)
  "Return the number in the triangular series at `I'."
  (if (= 0 i)
      0
    (+ i (series/triangular-number (number/dec i)))))

;; TODO: Improve performance.
;; TODO: Consider creating a stream protocol with `stream/next' and implement
;; this using that.
(defun series/triangular (n)
  "Return the first `N' numbers of a triangular series starting at 0."
  (if (= 0 n)
      '()
    (list/reverse
     (list/cons (series/triangular-number (number/dec n))
                (list/reverse
                 (series/triangular (number/dec n)))))))

(defun series/catalan-number (i)
  "Return the catalan number in the series at `I'."
  (if (= 0 i)
      1
    (/ (number/factorial (* 2 i))
       (* (number/factorial (number/inc i))
          (number/factorial i)))))

(defun series/catalan (n)
  "Return the first `N' numbers in a catalan series."
  (->> (series/range 0 (number/dec n))
       (list/map #'series/catalan-number)))

(provide 'series)
;;; series.el ends here
