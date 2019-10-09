;;; random.el --- Functions for working with randomness -*- lexical-binding: t -*-
;; Author: William Carroll <wpcarro@gmail.com>

;;; Commentary:
;; Functions for working with randomness.  Some of this code is not as
;; functional as I'd like from.

;;; Code:

(require 'prelude)
(require 'number)
(require 'math)
(require 'series)
(require 'list)
(require 'set)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Library
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun random/int (x)
  "Return a random integer from 0 to `X'."
  (random x))

;; TODO: Make this work with sequences instead of lists.
(defun random/choice (xs)
  "Return a random element of `XS'."
  (let ((ct (list/length xs)))
    (list/get
     (random/int ct)
     xs)))

(defun random/boolean? ()
  "Randonly return t or nil."
  (random/choice (list t nil)))

;; TODO: This may not work if any of these generate numbers like 0, 1, etc.
(defun random/uuid ()
  "Return a generated UUID string."
  (let ((eight  (number/dec (math/triangle-of-power :base 16 :power 8)))
        (four   (number/dec (math/triangle-of-power :base 16 :power 4)))
        (twelve (number/dec (math/triangle-of-power :base 16 :power 12))))
    (format "%x-%x-%x-%x-%x"
            (random/int eight)
            (random/int four)
            (random/int four)
            (random/int four)
            (random/int twelve))))

(defun random/token (length)
  "Return a randomly generated hexadecimal string of LENGTH."
  (->> (series/range 0 (number/dec length))
       (list/map (lambda (_) (format "%x" (random/int 15))))
       (list/join "")))

;; TODO: Support random/sample
(defun random/sample (n xs)
  "Return a randomly sample of list XS of size N."
  (prelude/assert (and (>= n 0) (< n (list/length xs))))
  (cl-labels ((do-sample
               (n xs y ys)
               (if (= n (set/count ys))
                   (->> ys
                        set/to-list
                        (list/map (lambda (i)
                                    (list/get i xs))))
                 (if (set/contains? y ys)
                     (do-sample n xs (random/int (list/length xs)) ys)
                   (do-sample n xs y (set/add y ys))))))
    (do-sample n xs (random/int (list/length xs)) (set/new))))

(provide 'random)
;;; random.el ends here
