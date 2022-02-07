;;; random.el --- Functions for working with randomness -*- lexical-binding: t -*-

;; Author: William Carroll <wpcarro@gmail.com>
;; Version: 0.0.1
;; Package-Requires: ((emacs "24"))

;;; Commentary:
;; Functions for working with randomness.  Some of this code is not as
;; functional as I'd like from.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dependencies
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'number)
(require 'math)
(require 'list)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Library
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun random-int (x)
  "Return a random integer from 0 to `X'."
  (random x))

;; TODO: Make this work with sequences instead of lists.
(defun random-choice (xs)
  "Return a random element of `XS'."
  (let ((ct (list-length xs)))
    (list-get
     (random-int ct)
     xs)))

(defun random-boolean? ()
  "Randonly return t or nil."
  (random-choice (list t nil)))

;; TODO: This may not work if any of these generate numbers like 0, 1, etc.
(defun random-uuid ()
  "Return a generated UUID string."
  (let ((eight  (number-dec (math-triangle-of-power :base 16 :power 8)))
        (four   (number-dec (math-triangle-of-power :base 16 :power 4)))
        (twelve (number-dec (math-triangle-of-power :base 16 :power 12))))
    (format "%x-%x-%x-%x-%x"
            (random-int eight)
            (random-int four)
            (random-int four)
            (random-int four)
            (random-int twelve))))

(defun random-token (length)
  "Return a randomly generated hexadecimal string of LENGTH."
  (->> (series/range 0 (number-dec length))
       (list-map (lambda (_) (format "%x" (random-int 15))))
       (list-join "")))

(provide 'random)
;;; random.el ends here
