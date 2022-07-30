;;; math.el --- Math stuffs -*- lexical-binding: t -*-

;; Author: William Carroll <wpcarro@gmail.com>
;; Version: 0.0.1
;; Package-Requires: ((emacs "24.3"))

;;; Commentary:
;; Containing some useful mathematical functions.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dependencies
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'dash)
(require 'maybe)
(require 'cl-lib)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst math-pi pi
  "The number pi.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: Support all three arguments.
;; Int -> Int -> Int -> Boolean
(cl-defun math-triangle-of-power (&key base power result)
  (cond
   ((-all? #'maybe-some? (list base power result))
    (error "All three arguments should not be set"))
   ((-all? #'maybe-some? (list power result))
    (message "power and result"))
   ((-all? #'maybe-some? (list base result))
    (log result base))
   ((-all? #'maybe-some? (list base power))
    (expt base power))
   (t
    (error "Two of the three arguments must be set"))))

(defun math-mod (x y)
  "Return X mod Y."
  (mod x y))

(defun math-exp (x y)
  "Return X raised to the Y."
  (expt x y))

(defun math-round (x)
  "Round X to nearest ones digit."
  (round x))

(defun math-floor (x)
  "Floor value X."
  (floor x))

(provide 'math)
;;; math.el ends here
