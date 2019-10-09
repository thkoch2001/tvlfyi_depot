;;; math.el --- Math stuffs -*- lexical-binding: t -*-
;; Author: William Carroll <wpcarro@gmail.com>

;;; Commentary:
;; Containing some useful mathematical functions.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dependencies
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'maybe)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst math/pi pi
  "The number pi.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: Support all three arguments.
;; Int -> Int -> Int -> Boolean
(cl-defun math/triangle-of-power (&key base power result)
  ;; TODO: Assert two of three are set.
  (cond
   ((maybe/some? base power result)
    (error "All three arguments should not be set"))
   ((maybe/some? power result)
    (message "power and result"))
   ((maybe/some? base result)
    (log result base))
   ((maybe/some? base power)
    (expt base power))
   (t
    (error "Two of the three arguments must be set"))))

(defun math/mod (x y)
  "Return X mod Y."
  (mod x y))

(defun math/exp (x y)
  "Return X raised to the Y."
  (expt x y))

(defun math/round (x)
  "Round X to nearest ones digit."
  (round x))

(defun math/floor (x)
  "Floor value X."
  (floor x))

(provide 'math)
;;; math.el ends here
