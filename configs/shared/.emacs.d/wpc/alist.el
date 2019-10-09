;;; alist.el --- Interface for working with associative lists -*- lexical-binding: t -*-
;; Author: William Carroll <wpcarro@gmail.com>

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

;; Dependencies:

;; TODO: Consider dropping explicit dependency white-listing since all of these
;; should be available in my Emacs.  The problem arises when this library needs
;; to be published, in which case, something like Nix and a build process could
;; possible insert the necessary require statements herein.  Not sure how I feel
;; about this though.
(require 'maybe)
(require 'macros)
(require 'dash)
(require 'tuple)
(require 'maybe)

;;; Code:

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

(defun alist/new ()
  "Return a new, empty alist."
  '())

;; Create
;; TODO: See if this mutates.
(defun alist/set (k v xs)
  "Set K to V in XS."
  (if (alist/has-key? k xs)
      (progn
        (setf (alist-get k xs) v)
        xs)
    (list/cons `(,k . ,v) xs)))

(defun alist/set! (k v xs)
  "Set K to V in XS mutatively.
Note that this doesn't append to the alist in the way that most alists handle
  writing.  If the k already exists in XS, it is overwritten."
  (map-delete xs k)
  (map-put xs k v))

;; Read
(defun alist/get (k xs)
  "Return the value at K in XS; otherwise, return nil.
Returns the first occurrence of K in XS since alists support multiple entries."
  (cdr (assoc k xs)))

(defun alist/get-entry (k xs)
  "Return the first key-value pair at K in XS."
  (assoc k xs))

;; Update
;; TODO: Add warning about only the first occurrence being updated in the
;; documentation.
(defun alist/update (k f xs)
  "Apply F to the value stored at K in XS.
If `K' is not in `XS', this function errors.  Use `alist/upsert' if you're
interested in inserting a value when a key doesn't already exist."
  (if (maybe/nil? (alist/get k xs))
      (error "Refusing to update: key does not exist in alist")
    (alist/set k (funcall f (alist/get k xs)) xs)))

(defun alist/update! (k f xs)
  "Call F on the entry at K in XS.
Mutative variant of `alist/update'."
  (alist/set! k (funcall f (alist/get k xs))xs))

;; TODO: Support this.
(defun alist/upsert (k v f xs)
  "If K exists in `XS' call `F' on the value otherwise insert `V'."
  (if (alist/get k xs)
      (alist/update k f xs)
    (alist/set k v xs)))

;; Delete
;; TODO: Make sure `delete' and `remove' behave as advertised in the Elisp docs.
(defun alist/delete (k xs)
  "Deletes the entry of K from XS.
This only removes the first occurrence of K, since alists support multiple
  key-value entries.  See `alist/delete-all' and `alist/dedupe'."
  (remove (assoc k xs) xs))

(defun alist/delete! (k xs)
  "Delete the entry of K from XS.
Mutative variant of `alist/delete'."
  (delete (assoc k xs) xs))

;; Additions to the CRUD API
;; TODO: Implement this function.
(defun alist/dedupe-keys (xs)
  "Remove the entries in XS where the keys are `equal'.")

(defun alist/dedupe-entries (xs)
  "Remove the entries in XS where the key-value pair are `equal'."
  (delete-dups xs))

(defun alist/keys (xs)
  "Return a list of the keys in XS."
  (mapcar 'car xs))

(defun alist/values (xs)
  "Return a list of the values in XS."
  (mapcar 'cdr xs))

(defun alist/has-key? (k xs)
  "Return t if XS has a key `equal' to K."
  (maybe/some? (assoc k xs)))

(defun alist/has-value? (v xs)
  "Return t if XS has a value of V."
  (maybe/some? (rassoc v xs)))

(defun alist/count (xs)
  "Return the number of entries in XS."
  (length xs))

(defun alist/reduce (acc f xs)
  "Return a new alist by calling, F, on k v and ACC from XS.
F should return a tuple.  See tuple.el for more information."
  (->> (alist/keys xs)
       (list/reduce acc
                    (lambda (k acc)
                      (funcall f k (alist/get k xs) acc)))))

(defun alist/merge (a b)
  "Return a new alist with a merge of alists, A and B.
In this case, the last writer wins, which is B."
  (alist/reduce a #'alist/set b))

;; TODO: Support `-all' variants like:
;; - get-all
;; - delete-all
;; - update-all

;; Scratch-pad
(macros/comment
 (progn
   (setq person '((first-name . "William")
                  (first-name . "William")
                  (last-name  . "Carroll")
                  (last-name  . "Another")))
   (alist/set 'last-name "Van Gogh" person)
   (alist/get 'last-name person)
   (alist/update 'last-name (lambda (x) "whoops") person)
   (alist/delete 'first-name person)
   (alist/keys person)
   (alist/values person)
   (alist/count person)
   (alist/has-key? 'first-name person)
   (alist/has-value? "William" person)
   ;; (alist/dedupe-keys person)
   (alist/dedupe-entries person)
   (alist/count person)))

;; Tests

;; TODO: Support test cases for the entire API.

(provide 'alist)
;;; alist.el ends here
