;;; maybe.el --- Library for dealing with nil values -*- lexical-binding: t -*-
;; Author: William Carroll <wpcarro@gmail.com>

;;; Commentary:
;; Inspired by Elm's Maybe library.
;;
;; For now, a Nothing value will be defined exclusively as a nil value.  I'm
;; uninterested in supported falsiness in this module even at risk of going
;; against the LISP grain.
;;
;; I'm avoiding introducing a struct to handle the creation of Just and Nothing
;; variants of Maybe.  Perhaps this is a mistake in which case this file would
;; be more aptly named nil.el.  I may change that.  Because of this limitation,
;; functions in Elm's Maybe library like andThen, which is the monadic bind for
;; the Maybe type, doesn't have a home here since we cannot compose multiple
;; Nothing or Just values without a struct or some other construct.
;;
;; Possible names for the variants of a Maybe.
;; None    | Some
;; Nothing | Something
;; None    | Just
;; Nil     | Set
;;
;; NOTE: In Elisp, values like '() (i.e. the empty list) are aliases for nil.
;; What else in Elisp is an alias in this way?
;; Examples:
;; TODO: Provide examples of other nil types in Elisp.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dependencies
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'prelude)
(require 'list)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar maybe/test? t
  "When t, run the test suite defined herein.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Library
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun maybe/nil? (x)
  "Return t if X is nil."
  (eq nil x))

(defun maybe/some? (x)
  "Return t when X is non-nil."
  (not (maybe/nil? x)))

(defun maybe/nils? (&rest xs)
  "Return t if all XS are nil."
  (list/all? #'maybe/nil? xs))

(defun maybe/somes? (&rest xs)
  "Return t if all XS are non-nil."
  (list/all? #'maybe/some? xs))

(defun maybe/default (default x)
  "Return DEFAULT when X is nil."
  (if (maybe/nil? x) default x))

(defun maybe/map (f x)
  "Apply F to X if X is not nil."
  (if (maybe/some? x)
      (funcall f x)
    x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when maybe/test?
  ;; nil?
  (prelude/assert (maybe/nil? nil))
  (prelude/refute (maybe/nil? t))
  ;; some?
  (prelude/assert (maybe/some? 10))
  (prelude/refute (maybe/some? nil))
  ;; nils?
  (prelude/assert (maybe/nils? nil nil nil nil))
  (prelude/refute (maybe/nils? nil t nil t))
  ;; somes?
  (prelude/assert (maybe/somes? t 10 '(1 2 3) "some"))
  (prelude/refute (maybe/somes? t nil '(1 2 3) "some"))
  ;; default
  (prelude/assert
   (and (= 0 (maybe/default 5 0))
        (= 5 (maybe/default 5 nil))))
  ;; map
  (prelude/assert
   (and (= 2 (maybe/map #'1+ 1))
        (eq nil (maybe/map #'1+ nil)))))

(provide 'maybe)
;;; maybe.el ends here
