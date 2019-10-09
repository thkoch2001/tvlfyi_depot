;;; iso.el --- Isomorphisms in Elisp -*- lexical-binding: t -*-
;; Author: William Carroll <wpcarro@gmail.com>

;;; Commentary:
;; Providing basic isomorphisms to improve code quality.

;;; Code:

(require 'dotted)
(require 'tuple)
(require 'symbol)
(require 'string)
(require 'list)
(require 'alist)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Library
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl-defstruct iso to from x)

(defconst iso/whitelist
  '((dotted . tuple)
    (symbol . string))
  "Alist representing supported isomorphisms.")

(defconst iso/vertices
  (list/concat (alist/keys iso/whitelist)
               (alist/values iso/whitelist))
  "List of all of the vertices in the iso graph.")

(defun iso/classify (x)
  "Return type of X."
  (cond
   ((string/instance? x) 'string)
   ((symbol/instance? x) 'symbol)
   ((dotted/instance? x) 'dotted)
   ((tuple/instance? x)  'tuple)))

(cl-defun iso/exists? (to from)
  "Return t if an isomorphism of TO to FROM exists."
  ;; TODO: All of this can be improved modelling this with a graph.
  (cond
   ;; to -> from
   ((list/contains? to (alist/keys iso/whitelist))
    (list/contains? from (alist/values iso/whitelist)))
   ;; from -> to
   ((list/contains? from (alist/keys iso/whitelist))
    (list/contains? to (alist/values iso/whitelist)))
   ;; doesn't exist
   (t nil)))

(progn
  (prelude/assert
   (iso/exists? 'symbol 'string))
  (prelude/assert
   (iso/exists? 'dotted 'tuple))
  (prelude/refute
   (iso/exists? 'dotted 'symbol))
  (prelude/refute
   (iso/exists? 'symbol 'list)))

;; TODO: Model this as a graph.
(defconst iso/morphisms
  '((string .
            '(symbol #')
     ))
  (list (:from 'string :to 'symbol :fn #'intern)
        (:from 'symbol :to 'string :fn #'symbol-name)
        )
  "")

(defun iso/to (f x)
  "Apply F to X's to."
  (->> x
       iso-to))

(->> (iso/new "william" :to 'symbol)
     (iso/as-to #'symbol-name)
     )

(cl-defun iso/new (x &keys to)
  "Create a new isomorphism of X mapping to TO."
  (let ((from (iso/classify x)))
    (prelude/assert (iso/exists? to from))
    (make-iso :from from
              :to to
              :x x)))

(macros/comment
 (iso/new "william" :to 'symbol)
 (iso/new '(one . two) :to 'tuple))

(provide 'iso)
;;; iso.el ends here
