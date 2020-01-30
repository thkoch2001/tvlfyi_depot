;;; stack.el --- Working with stacks in Elisp -*- lexical-binding: t -*-
;; Author: William Carroll <wpcarro@gmail.com>

;;; Commentary:
;; A stack is a LIFO queue.
;; The design goal here is to expose an intuitive API for working with stacks in
;; non-mutative way.
;;
;; TODO: Consider naming a Functor instance "Mappable."
;; TODO: Consider naming a Foldable instance "Reduceable."
;;
;; TODO: Consider implementing an instance for Mappable.
;; TODO: Consider implementing an instance for Reduceable.

;;; Code:

(require 'list)

(cl-defstruct stack xs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Create
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun stack/new ()
  "Create an empty stack."
  (make-stack :xs '()))

(defun stack/from-list (xs)
  "Create a new stack from the list, `XS'."
  (list/reduce (stack/new) #'stack/push xs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Read
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun stack/peek (xs)
  "Look at the top element of `XS' without popping it off."
  (->> xs
       stack-xs
       list/head))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Update
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun stack/push (x xs)
  "Push `X' on `XS'."
  (struct/update stack
                 xs
                 (>> (list/cons x))
                 xs))

;; TODO: How to return something like {(list/head xs), (list/tail xs)} in Elixir
;; TODO: How to handle popping from empty stacks?
(defun stack/pop (xs)
  "Return the stack, `XS', without the top element.
Since I cannot figure out a nice way of return tuples in Elisp, if you want to
look at the first element, use `stack/peek' before running `stack/pop'."
  (struct/update stack
                 xs
                 (>> list/tail)
                 xs))

(defun stack/map-top (f xs)
  "Apply F to the top element of XS."
  (->> xs
       stack/pop
       (stack/push (funcall f (stack/peek xs)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Miscellaneous
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun stack/to-list (xs)
  "Return XS as a list.
The round-trip property of `stack/from-list' and `stack/to-list' should hold."
  (->> xs
       stack-xs
       list/reverse))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Predicates
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: Create a macro that wraps `cl-defstruct' that automatically creates
;; things like `new', `instance?'.
(defun stack/instance? (xs)
  "Return t if XS is a stack."
  (stack-p xs))

(provide 'stack)
;;; stack.el ends here
