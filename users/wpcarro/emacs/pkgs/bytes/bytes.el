;;; bytes.el --- Working with byte values -*- lexical-binding: t -*-

;; Author: William Carroll <wpcarro@gmail.com>
;; Version: 0.0.1
;; Package-Requires: ((emacs "24.3"))

;;; Commentary:
;; Functions to help with human-readable representations of byte values.
;;
;; Usage:
;; See the test cases for example usage.  Or better yet, I should use a type of
;; structured documentation that would allow me to expose a view into the test
;; suite here.  Is this currently possible in Elisp?
;;
;; API:
;; - serialize :: Integer -> String
;;
;; Wish list:
;; - Rounding: e.g. (bytes (* 1024 1.7)) => "2KB"

;;; Code:

;; TODO: Support -ibabyte variants like Gibibyte (GiB).

;; Ranges:
;;  B: [   0,  1e3)
;; KB: [ 1e3,  1e6)
;; MB: [ 1e6,  1e6)
;; GB: [ 1e9, 1e12)
;; TB: [1e12, 1e15)
;; PB: [1e15, 1e18)
;;
;; Note: I'm currently not support exabytes because that causes the integer to
;;  overflow.  I imagine a larger integer type may exist, but for now, I'll
;;  treat this as a YAGNI.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dependencies
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'tuple)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst bytes-kb (expt 2 10)
  "Number of bytes in a kilobyte.")

(defconst bytes-mb (expt 2 20)
  "Number of bytes in a megabytes.")

(defconst bytes-gb (expt 2 30)
  "Number of bytes in a gigabyte.")

(defconst bytes-tb (expt 2 40)
  "Number of bytes in a terabyte.")

(defconst bytes-pb (expt 2 50)
  "Number of bytes in a petabyte.")

(defconst bytes-eb (expt 2 60)
  "Number of bytes in an exabyte.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun bytes-classify (x)
  "Return unit that closest fits byte count, X."
  (cond
   ((and (>= x 0)        (< x bytes-kb))     'byte)
   ((and (>= x bytes-kb) (< x bytes-mb)) 'kilobyte)
   ((and (>= x bytes-mb) (< x bytes-gb)) 'megabyte)
   ((and (>= x bytes-gb) (< x bytes-tb)) 'gigabyte)
   ((and (>= x bytes-tb) (< x bytes-pb)) 'terabyte)
   ((and (>= x bytes-pb) (< x bytes-eb)) 'petabyte)))

(defun bytes-to-string (x)
  "Convert integer X into a human-readable string."
  (let ((base-and-unit
         (pcase (bytes-classify x)
           ('byte     (tuple-from        1 "B"))
           ('kilobyte (tuple-from bytes-kb "KB"))
           ('megabyte (tuple-from bytes-mb "MB"))
           ('gigabyte (tuple-from bytes-gb "GB"))
           ('terabyte (tuple-from bytes-tb "TB"))
           ('petabyte (tuple-from bytes-pb "PB")))))
    (format "%d%s"
            (round x (tuple-first base-and-unit))
            (tuple-second base-and-unit))))

(provide 'bytes)
;;; bytes.el ends here
