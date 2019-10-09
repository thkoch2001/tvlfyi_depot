;;; graph.el --- Working with in-memory graphs -*- lexical-binding: t -*-
;; Author: William Carroll <wpcarro@gmail.com>

;;; Commentary:
;;
;; Remember that there are optimal three ways to model a graph:
;; 1. Edge List
;; 2. Vertex Table (a.k.a. Neighbors Table)
;; 3. Adjacency Matrix
;;
;; I may call these "Edges", "Neighbors", "Adjacencies" to avoid verbose naming.
;; For now, I'm avoiding dealing with Adjacency Matrices as I don't have an
;; immediate use-case for them.  This is subject to change.
;;
;; There are also hybrid representations of graphs that combine the three
;; aforementioned models.  I believe Erlang's digraph module models graphs in
;; Erlang Term Storage (i.e. ETS) this way.
;; TODO: Verify this claim.
;;
;; Graphs can be weighted or unweighted.  They can also be directed or
;; undirected.
;; TODO: Create a table explaining all graph variants.
;;
;; TODO: Figure out the relationship of this module and tree.el, which should in
;; principle overlap.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dependencies
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'prelude)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Library
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; For now, I'll support storing *either* neighbors or edges in the graph struct
;; as long as both aren't set, since that introduces consistency issues.  I may
;; want to handle that use-case in the future, but not now.
(cl-defstruct graph neighbors edges)

;; TODO: How do you find the starting point for a topo sort?
(defun graph/sort (xs)
  "Return a topological sort of XS.")

(defun graph/from-edges (xs)
  "Create a graph struct from the Edge List, XS.
The user must pass in a valid Edge List since asserting on the shape of XS might
  be expensive."
  (make-graph :edges xs))

(defun graph/from-neighbors (xs)
  "Create a graph struct from a Neighbors Table, XS.
The user must pass in a valid Neighbors Table since asserting on the shape of
  XS might be expensive."
  (make-graph :neighbors xs))

(defun graph/instance? (xs)
  "Return t if XS is a graph struct."
  (graph-p xs))

;; TODO: Model each of the mapping functions into an isomorphism.
(defun graph/edges->neighbors (xs)
  "Map Edge List, XS, into a Neighbors Table."
  (prelude/assert (graph/instance? xs)))

(defun graph/neighbors->edges (xs)
  "Map Neighbors Table, XS, into an Edge List."
  (prelude/assert (graph/instance? xs)))

;; Below are three different models of the same unweighted, directed graph.

(defvar graph/edges
  '((a . b) (a . c) (a . e)
    (b . c) (b . d)
    (c . e)
    (d . f)
    (e . d) (e . f)))

(defvar graph/neighbors
  ((a b c e)
   (b c d)
   (c e)
   (d f)
   (e d g)
   (f)))

(provide 'graph)
;;; graph.el ends here
