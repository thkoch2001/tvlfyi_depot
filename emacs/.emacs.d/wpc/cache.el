;;; cache.el --- Caching things -*- lexical-binding: t -*-
;; Author: William Carroll <wpcarro@gmail.com>

;;; Commentary:
;; An immutable cache data structure.
;;
;; This is like a sideways stack, that you can pull values out from and re-push
;; to the top.  It'd be like a stack supporting push, pop, pull.
;;
;; This isn't a key-value data-structure like you might expect from a
;; traditional cache.  The name is subject to change, but the underlying idea of
;; a cache remains the same.
;;
;; Think about prescient.el, which uses essentially an LRU cache integrated into
;; counsel to help create a "clairovoyant", self-organizing list.
;;
;; Use-cases:
;; - Keeps an cache of workspaces sorted as MRU with an LRU eviction strategy.

;;; Code:

(require 'prelude)
(require 'struct)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Library
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl-defstruct cache xs)

;; TODO: Prefer another KBD for yasnippet form completion than company-mode's
;; current KBD.

(defun cache/from-list (xs)
  "Turn list, XS, into a cache."
  (make-cache :xs xs))

(defun cache/contains? (x xs)
  "Return t if X in XS."
  (->> xs
       cache-xs
       (list/contains? x)))

(defun cache/touch (x xs)
  "Ensure value X in cache, XS, is front of the list.
If X isn't in XS (using `equal'), insert it at the front."
  (struct/update
   cache
   xs
   (>> (list/reject (lambda (y) (equal x y)))
       (list/cons x))
   xs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(progn
  (let ((cache (cache/from-list '("chicken" "nugget"))))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; contains?/2
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (prelude/refute
     (cache/contains? "turkey" cache))
    (prelude/assert
     (cache/contains? "chicken" cache))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; touch/2
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (prelude/assert
     (equal
      (cache/touch "nugget" cache)
      (cache/from-list '("nugget" "chicken"))))
    (prelude/assert
     (equal
      (cache/touch "spicy" cache)
      (cache/from-list '("spicy" "chicken" "nugget"))))))

(provide 'cache)
;;; cache.el ends here
