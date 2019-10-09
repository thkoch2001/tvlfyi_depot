;;; number.el --- Functions for working with numbers -*- lexical-binding: t -*-
;; Author: William Carroll <wpcarro@gmail.com>

;;; Commentary:
;;
;; Classifications of numbers:
;; - Natural: (a.k.a positive integers, counting numbers); {1, 2, 3, ... }
;;
;; - Whole: Natural Numbers, plus zero; {0, 1, 2, 3, ...}
;;
;; - Integers: Whole numbers plus all the negatives of the natural numbers;
;;   {... , -2, -1, 0, 1, 2, ...}
;;
;; - Rational numbers: (a.k.a. fractions) where the top and bottom numbers are
;;   integers; e.g., 1/2, 3/4, 7/2, ⁻4/3, 4/1.  Note: The denominator cannot be
;;   0, but the numerator can be.
;;
;; - Real numbers: All numbers that can be written as a decimal.  This includes
;;   fractions written in decimal form e.g., 0.5, 0.75 2.35, ⁻0.073, 0.3333, or
;;   2.142857. It also includes all the irrational numbers such as π, √2 etc.
;;   Every real number corresponds to a point on the number line.
;;
;; The functions defined herein attempt to capture the mathematical definitions
;; of numbers and their classifications as defined above.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dependencies
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'prelude)
(require 'dash)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Library
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst number/test? t
  "When t, run the test suite defined herein.")

;; TODO: What about int.el?

;; TODO: How do we handle a number typeclass?

(defun number/positive? (x)
  "Return t if `X' is a positive number."
  (> x 0))

(defun number/negative? (x)
  "Return t if `X' is a positive number."
  (< x 0))

;; TODO: Don't rely on this. Need to have 10.0 and 10 behave similarly.
(defun number/float? (x)
  "Return t if `X' is a floating point number."
  (floatp x))

(defun number/natural? (x)
  "Return t if `X' is a natural number."
  (and (number/positive? x)
       (not (number/float? x))))

(defun number/whole? (x)
  "Return t if `X' is a whole number."
  (or (= 0 x)
      (number/natural? x)))

(defun number/integer? (x)
  "Return t if `X' is an integer."
  (or (number/whole? x)
      (number/natural? (- x))))

;; TODO: How defensive should these guards be?  Should we assert that the inputs
;; are integers before checking evenness or oddness?

;; TODO: Look up Runar (from Unison) definition of handling zero as even or odd.

;; TODO: How should rational numbers be handled? Lisp is supposedly famous for
;; its handling of rational numbers.
;; TODO: `calc-mode' supports rational numbers as "1:2" meaning "1/2"
;; (defun number/rational? (x))

;; TODO: Can or should I support real numbers?
;; (defun number/real? (x))

(defun number/even? (x)
  "Return t if `X' is an even number."
  (or (= 0 x)
      (= 0 (mod x 2))))

(defun number/odd? (x)
  "Return t if `X' is an odd number."
  (not (number/even? x)))

(defun number/dec (x)
  "Subtract one from `X'.
While this function is undeniably trivial, I have unintentionally done (- 1 x)
  when in fact I meant to do (- x 1) that I figure it's better for this function
  to exist, and for me to train myself to reach for it and its inc counterpart."
  (- x 1))

(defun number/inc (x)
  "Add one to `X'."
  (+ x 1))

;; TODO: Does this belong in a math module?  Is math too vague?  Or is number
;; too vague?
(defun number/factorial (x)
  "Return factorial of `X'."
  (cond
   ((number/negative? x) (error "Will not take factorial of negative numbers"))
   ((= 0 x) 1)
   ;; NOTE: Using `series/range' introduces a circular dependency because:
   ;; series -> number -> series.  Conceptually, however, this should be
   ;; perfectly acceptable.
   (t (->> (series/range 1 x)
           (list/reduce 1 #'*)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when number/test?
  (prelude/assert
   (number/positive? 10))
  (prelude/assert
   (number/natural? 10))
  (prelude/assert
   (number/whole? 10))
  (prelude/assert
   (number/whole? 0))
  (prelude/assert
   (number/integer? 10))
  (prelude/assert
   (= 120 (number/factorial 5)))
  (prelude/assert
   (number/even? 6))
  (prelude/refute
   (number/odd? 6))
  (prelude/refute
   (number/positive? -10))
  (prelude/refute
   (number/natural? 10.0))
  (prelude/refute
   (number/natural? -10))
  (prelude/refute
   (number/natural? -10.0)))

(provide 'number)
;;; number.el ends here
