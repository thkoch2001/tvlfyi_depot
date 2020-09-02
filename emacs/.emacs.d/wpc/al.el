;;; al.el --- Interface for working with associative lists -*- lexical-binding: t -*-

;; Author: William Carroll <wpcarro@gmail.com>
;; Version: 0.0.1
;; URL: https://git.wpcarro.dev/wpcarro/briefcase
;; Package-Requires: ((emacs "25.1"))

;;; Commentary:
;; Firstly, a rant:
;; In most cases, I find Elisp's APIs to be confusing.  There's a mixture of
;; overloaded functions that leak the implementation details (TODO: provide an
;; example of this.) of the abstract data type, which I find privileges those
;; "insiders" who spend disproportionately large amounts of time in Elisp land,
;; and other functions with little-to-no pattern about the order in which
;; arguments should be applied.  In theory, however, most of these APIs could
;; and should be much simpler.  This module represents a step in that direction.
;;
;; I'm modelling these APIs after Elixir's APIs.
;;
;; On my wishlist is to create protocols that will allow generic interfaces like
;; Enum protocols, etc.  Would be nice to abstract over...
;; - associative lists (i.e. alists)
;; - property lists (i.e. plists)
;; - hash tables
;; ...with some dictionary or map-like interface.  This will probably end up
;; being quite similar to the kv.el project but with differences at the API
;; layer.
;;
;; Similar libraries:
;; - map.el: Comes bundled with recent versions of Emacs.
;; - asoc.el: Helpers for working with alists.  asoc.el is similar to alist.el
;;   because it uses the "!" convention for signalling that a function mutates
;;   the underlying data structure.
;; - ht.el: Hash table library.
;; - kv.el: Library for dealing with key-value collections.  Note that map.el
;;   has a similar typeclass because it works with lists, hash-tables, or
;;   arrays.
;; - a.el: Clojure-inspired way of working with key-value data structures in
;; Elisp.  Works with alists, hash-tables, and sometimes vectors.
;;
;; Some API design principles:
;; - The "noun" (i.e. alist) of the "verb" (i.e. function) comes last to improve
;; composability with the threading macro (i.e. `->>') and to improve consumers'
;; intuition with the APIs.  Learn this once, know it always.
;;
;; - Every function avoids mutating the alist unless it ends with !.
;;
;; - CRUD operations will be named according to the following table:
;;   - "create" *and* "set"
;;   - "read"   *and* "get"
;;   - "update"
;;   - "delete" *and* "remove"
;;
;; For better or worse, all of this code expects alists in the form of:
;; ((first-name . "William") (last-name . "Carroll"))
;;
;; Special thanks to github.com/alphapapa/emacs-package-dev-handbook for some of
;; the idiomatic ways to update alists.
;;
;; TODO: Include a section that compares alist.el to a.el from
;; github.com/plexus/a.el.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dependencies:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'macros)
(require 'dash)
(require 'tuple)
(require 'maybe)

;; TODO: Support function aliases for:
;; - create/set
;; - read/get
;; - update
;; - delete/remove

;; Support mutative variants of functions with an ! appendage to their name.

;; Ensure that the same message about only updating the first occurrence of a
;; key is consistent throughout documentation using string interpolation or some
;; other mechanism.

;; TODO: Consider wrapping all of this with `(cl-defstruct alist xs)'.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst al-enable-tests? t
  "When t, run the test suite.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Library
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: Support a variadic version of this to easily construct alists.
(defun al-new ()
  "Return a new, empty alist."
  '())

;; Create
;; TODO: See if this mutates.
(defun al-set (k v xs)
  "Set K to V in XS."
  (if (al-has-key? k xs)
      (progn
        ;; Note: this is intentional `alist-get' and not `al-get'.
        (setf (alist-get k xs) v)
        xs)
    (list-cons `(,k . ,v) xs)))

(defun al-set! (k v xs)
  "Set K to V in XS mutatively.
Note that this doesn't append to the alist in the way that most alists handle
  writing.  If the k already exists in XS, it is overwritten."
  (map-delete xs k)
  (map-put! xs k v))

;; Read
(defun al-get (k xs)
  "Return the value at K in XS; otherwise, return nil.
Returns the first occurrence of K in XS since alists support multiple entries."
  (cdr (assoc k xs)))

(defun al-get-entry (k xs)
  "Return the first key-value pair at K in XS."
  (assoc k xs))

;; Update
;; TODO: Add warning about only the first occurrence being updated in the
;; documentation.
(defun al-update (k f xs)
  "Apply F to the value stored at K in XS.
If `K' is not in `XS', this function errors.  Use `al-upsert' if you're
interested in inserting a value when a key doesn't already exist."
  (if (not (al-has-key? k xs))
      (error "Refusing to update: key does not exist in alist")
    (al-set k (funcall f (al-get k xs)) xs)))

(defun al-update! (k f xs)
  "Call F on the entry at K in XS.
Mutative variant of `al-update'."
  (al-set! k (funcall f (al-get k xs))xs))

;; TODO: Support this.
(defun al-upsert (k v f xs)
  "If K exists in `XS' call `F' on the value otherwise insert `V'."
  (if (al-has-key? k xs)
      (al-update k f xs)
    (al-set k v xs)))

;; Delete
;; TODO: Make sure `delete' and `remove' behave as advertised in the Elisp docs.
(defun al-delete (k xs)
  "Deletes the entry of K from XS.
This only removes the first occurrence of K, since alists support multiple
  key-value entries.  See `al-delete-all' and `al-dedupe'."
  (remove (assoc k xs) xs))

(defun al-delete! (k xs)
  "Delete the entry of K from XS.
Mutative variant of `al-delete'."
  (delete (assoc k xs) xs))

;; Additions to the CRUD API
;; TODO: Implement this function.
(defun al-dedupe-keys (xs)
  "Remove the entries in XS where the keys are `equal'.")

(defun al-dedupe-entries (xs)
  "Remove the entries in XS where the key-value pair are `equal'."
  (delete-dups xs))

(defun al-keys (xs)
  "Return a list of the keys in XS."
  (mapcar 'car xs))

(defun al-values (xs)
  "Return a list of the values in XS."
  (mapcar 'cdr xs))

(defun al-has-key? (k xs)
  "Return t if XS has a key `equal' to K."
  (maybe-some? (assoc k xs)))

(defun al-has-value? (v xs)
  "Return t if XS has a value of V."
  (maybe-some? (rassoc v xs)))

(defun al-count (xs)
  "Return the number of entries in XS."
  (length xs))

;; TODO: Should I support `al-find-key' and `al-find-value' variants?
(defun al-find (p xs)
  "Find an element in XS.

Apply a predicate fn, P, to each key and value in XS and return the key of the
first element that returns t."
  (let ((result (list-find (lambda (x) (funcall p (car x) (cdr x))) xs)))
    (if result
        (car result)
      nil)))

(defun al-map-keys (f xs)
  "Call F on the values in XS, returning a new alist."
  (list-map (lambda (x)
              `(,(funcall f (car x)) . ,(cdr x)))
            xs))

(defun al-map-values (f xs)
  "Call F on the values in XS, returning a new alist."
  (list-map (lambda (x)
              `(,(car x) . ,(funcall f (cdr x))))
            xs))

(defun al-reduce (acc f xs)
  "Return a new alist by calling F on k v and ACC from XS.
F should return a tuple.  See tuple.el for more information."
  (->> (al-keys xs)
       (list-reduce acc
                    (lambda (k acc)
                      (funcall f k (al-get k xs) acc)))))

(defun al-merge (a b)
  "Return a new alist with a merge of alists, A and B.
In this case, the last writer wins, which is B."
  (al-reduce a #'al-set b))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when al-enable-tests?
  (prelude-assert
   (equal '((2 . one)
            (3 . two))
          (al-map-keys #'1+
                          '((1 . one)
                            (2 . two)))))
  (prelude-assert
   (equal '((one . 2)
            (two . 3))
          (al-map-values #'1+
                            '((one . 1)
                              (two . 2))))))


;; TODO: Support test cases for the entire API.

(provide 'al)
;;; al.el ends here
