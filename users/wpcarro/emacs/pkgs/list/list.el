;;; list.el --- Functions for working with lists -*- lexical-binding: t -*-

;; Author: William Carroll <wpcarro@gmail.com>
;; Version: 0.0.1
;; Package-Requires: ((emacs "24"))

;;; Commentary:
;; Since I prefer having the `list-' namespace, I wrote this module to wrap many
;; of the functions that are defined in the the global namespace in ELisp.  I
;; sometimes forget the names of these functions, so it's nice for them to be
;; organized like this.
;;
;; Motivation:
;; Here are some examples of function names that I cannot tolerate:
;; - `car': Return the first element (i.e. "head") of a linked list
;; - `cdr': Return the tail of a linked list

;; As are most APIs for standard libraries that I write, this is heavily
;; influenced by Elixir's standard library.
;;
;; Elixir's List library:
;; - ++/2
;; - --/2
;; - hd/1
;; - tl/1
;; - in/2
;; - length/1
;;
;; Similar libraries:
;; - dash.el: Functional library that mimmicks Clojure.  It is consumed herein.
;; - list-utils.el: Utility library that covers things that dash.el may not
;;   cover.
;;   stream.el: Elisp implementation of streams, "implemented as delayed
;;   evaluation of cons cells."

;; TODO: Consider naming this file linked-list.el.

;; TODO: Support module-like macro that auto-namespaces functions.

;; TODO: Consider wrapping most data structures like linked-lists,
;; associative-lists, etc in a `cl-defstruct', so that the dispatching by type
;; can be nominal instead of duck-typing.  I'm not sure if this is a good idea
;; or not.  If I do this, I should provide isomorphisms to map between idiomatic
;; ways of working with Elisp data structures and my wrapped variants.

;; TODO: Are function aliases/synonyms even a good idea?  Or do they just
;; bloat the API unnecessarily?

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dependencies
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'dash)
(require 'maybe)
(require 'set)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Library
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun list-new ()
  "Return a new, empty list."
  '())

(defun list-concat (&rest lists)
  "Joins `LISTS' into on list."
  (apply #'-concat lists))

(defun list-join (joint xs)
  "Join a list of strings, XS, with JOINT."
  (if (list-empty? xs)
      ""
    (list-reduce (list-first xs)
                 (lambda (x acc)
                   (format "%s%s%s" acc joint x))
                 (list-tail xs))))

(defun list-length (xs)
  "Return the number of elements in `XS'."
  (length xs))

(defun list-get (i xs)
  "Return the value in `XS' at `I', or nil."
  (nth i xs))

(defun list-head (xs)
  "Return the head of `XS'."
  (car xs))

;; TODO: Learn how to write proper function aliases.
(defun list-first (xs)
  "Alias for `list-head' for `XS'."
  (list-head xs))

(defun list-tail (xs)
  "Return the tail of `XS'."
  (cdr xs))

(defun list-reverse (xs)
  "Reverses `XS'."
  (reverse xs))

(defun list-cons (x xs)
  "Add `X' to the head of `XS'."
  (cons x xs))

;; map, filter, reduce

;; TODO: Create function adapters like swap.
;; (defun adapter/swap (f)
;;   "Return a new function that wraps `F' and swaps the arguments."
;;   (lambda (a b)
;;     (funcall f b a)))

;; TODO: Make this function work.
(defun list-reduce (acc f xs)
  "Return over `XS' calling `F' on an element in `XS'and `ACC'."
  (-reduce-from (lambda (acc x) (funcall f x acc)) acc xs))

(defun list-map (f xs)
  "Call `F' on each element of `XS'."
  (-map f xs))

(defun list-map-indexed (f xs)
  "Call `F' on each element of `XS' along with its index."
  (-map-indexed (lambda (i x) (funcall f x i)) xs))

(defun list-filter (p xs)
  "Return a subset of XS where predicate P returned t."
  (list-reverse
   (list-reduce
    '()
    (lambda (x acc)
      (if (funcall p x)
          (list-cons x acc)
        acc))
    xs)))

(defun list-reject (p xs)
  "Return a subset of XS where predicate of P return nil."
  (list-filter (lambda (x) (not (funcall p x))) xs))

(defun list-find (p xs)
  "Return the first x in XS that passes P or nil."
  (-find p xs))

;; TODO: Support dedupe.
;; TODO: Should we call this unique? Or distinct?

;; TODO: Add tests.
(defun list-dedupe-adjacent (xs)
  "Return XS without adjacent duplicates."
  (list-reverse
   (list-reduce (list (list-first xs))
                (lambda (x acc)
                  (if (equal x (list-first acc))
                      acc
                    (list-cons x acc)))
                xs)))

(defun list-chunk (n xs)
  "Chunk XS into lists of size N."
  (if (> n (length xs))
      (list xs)
    (->> xs
         (list-reduce '(:curr () :result ())
                      (lambda (x acc)
                        (let ((curr (plist-get acc :curr))
                              (result (plist-get acc :result)))
                          (if (= (- n 1) (length curr))
                              `(:curr () :result ,(list-cons (list-reverse (list-cons x curr)) result))
                            `(:curr ,(list-cons x curr) :result ,result)))))
         (funcall (lambda (xs)
                    (let ((curr (plist-get xs :curr))
                          (result (plist-get xs :result)))
                      (if curr (list-cons curr result)) result)))
         list-reverse)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Predicates
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun list-instance? (xs)
  "Return t if `XS' is a list.
Be leery of using this with things like alists.  Many data structures in Elisp
  are implemented using linked lists."
  (listp xs))

(defun list-empty? (xs)
  "Return t if XS are empty."
  (= 0 (list-length xs)))

(defun list-all? (p xs)
  "Return t if all `XS' pass the predicate, `P'."
  (-all? p xs))

(defun list-any? (p xs)
  "Return t if any `XS' pass the predicate, `P'."
  (-any? p xs))

(defun list-contains? (x xs)
  "Return t if X is in XS using `equal'."
  (maybe-some? (-contains? xs x)))

(defun list-xs-distinct-by? (f xs)
  "Return t if all elements in XS are distinct after applying F to each."
  (= (length xs)
     (->> xs (-map f) set-from-list set-count)))

(provide 'list)
;;; list.el ends here
