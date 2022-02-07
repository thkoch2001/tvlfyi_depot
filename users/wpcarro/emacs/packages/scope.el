;;; scope.el --- Work with a scope data structure -*- lexical-binding: t -*-

;; Author: William Carroll <wpcarro@gmail.com>
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1"))

;;; Commentary:
;; Exposing an API for working with a scope data structure in a non-mutative
;; way.
;;
;; What's a scope?  Think of a scope as a stack of key-value bindings.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dependencies
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'al)
(require 'stack)
(require 'struct)
(require '>)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Create
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl-defstruct scope scopes)

(defun scope-new ()
  "Return an empty scope."
  (make-scope :scopes (->> (stack-new)
                           (stack-push (al-new)))))

(defun scope-flatten (xs)
  "Return a flattened representation of the scope, XS.
The newest bindings eclipse the oldest."
  (->> xs
       scope-scopes
       stack-to-list
       (list-reduce (al-new)
                    (lambda (scope acc)
                      (al-merge acc scope)))))

(defun scope-push-new (xs)
  "Push a new, empty scope onto XS."
  (struct-update scope
                 scopes
                 (>-> (stack-push (al-new)))
                 xs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Read
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun scope-get (k xs)
  "Return K from XS if it's in scope."
  (->> xs
       scope-flatten
       (al-get k)))

(defun scope-current (xs)
  "Return the newest scope from XS."
  (let ((xs-copy (copy-scope xs)))
    (->> xs-copy
         scope-scopes
         stack-peek)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Update
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun scope-set (k v xs)
  "Set value, V, at key, K, in XS for the current scope."
  (struct-update scope
                 scopes
                 (>-> (stack-map-top (>-> (al-set k v))))
                 xs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Delete
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun scope-pop (xs)
  "Return a new scope without the top element from XS."
  (->> xs
       scope-scopes
       stack-pop))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Predicates
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun scope-defined? (k xs)
  "Return t if K is in scope of XS."
  (->> xs
       scope-flatten
       (al-has-key? k)))

;; TODO: Find a faster way to write aliases like this.
(defun scope-instance? (xs)
  "Return t if XS is a scope struct."
  (scope-p xs))

(provide 'scope)
;;; scope.el ends here
