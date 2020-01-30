;;; tree.el --- Working with Trees -*- lexical-binding: t -*-
;; Author: William Carroll <wpcarro@gmail.com>

;;; Commentary:
;; Some friendly functions that hopefully will make working with trees cheaper
;; and therefore more appealing!
;;
;; Tree terminology:
;; - leaf: node with zero children.
;; - root: node with zero parents.
;; - depth: measures a node's distance from the root node.  This implies the
;;   root node has a depth of zero.
;; - height: measures the longest traversal from a node to a leaf.  This implies
;;   that a leaf node has a height of zero.
;; - balanced?
;;
;; Tree variants:
;; - binary: the maximum number of children is two.
;; - binary search: the maximum number of children is two and left sub-trees are
;;   lower in value than right sub-trees.
;; - rose: the number of children is variable.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dependencies
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'prelude)
(require 'list)
(require 'set)
(require 'tuple)
(require 'series)
(require 'random)
(require 'maybe)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Library
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl-defstruct tree xs)

(cl-defstruct node value children)

(cl-defun tree/node (value &optional children)
  "Create a node struct of VALUE with CHILDREN."
  (make-node :value value
             :children children))

(defun tree/reduce-breadth (acc f xs)
  "Reduce over XS breadth-first applying F to each x and ACC (in that order).
Breadth-first traversals guarantee to find the shortest path in a graph.
  They're typically more difficult to implement than DFTs and may also incur
  higher memory costs on average than their depth-first counterparts.")

;; TODO: Support :order as 'pre | 'in | 'post.
;; TODO: Troubleshoot why I need defensive (nil? node) check.
(defun tree/reduce-depth (acc f node)
  "Reduce over NODE depth-first applying F to each NODE and ACC.
F is called with each NODE, ACC, and the current depth.
Depth-first traversals have the advantage of typically consuming less memory
  than their breadth-first equivalents would have.  They're also typically
  easier to implement using recursion.  This comes at the cost of not
  guaranteeing to be able to find the shortest path in a graph."
  (cl-labels ((do-reduce-depth
               (acc f node depth)
               (let ((acc-new (funcall f node acc depth)))
                 (if (or (maybe/nil? node)
                         (tree/leaf? node))
                     acc-new
                   (list/reduce
                    acc-new
                    (lambda (node acc)
                      (tree/do-reduce-depth
                       acc
                       f
                       node
                       (number/inc depth)))
                    (node-children node))))))
    (do-reduce-depth acc f node 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun tree/height (xs)
  "Return the height of tree XS.")

;; TODO: Troubleshoot why need for (nil? node).  Similar misgiving
;; above.
(defun tree/leaf-depths (xs)
  "Return a list of all of the depths of the leaf nodes in XS."
  (list/reverse
   (tree/reduce-depth
    '()
    (lambda (node acc depth)
      (if (or (maybe/nil? node)
              (tree/leaf? node))
          (list/cons depth acc)
        acc))
    xs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generators
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: Consider parameterizing height, forced min-max branching, random
;; distributions, etc.

;; TODO: Bail out before stack overflowing by consider branching, current-depth.

(cl-defun tree/random (&optional (value-fn (lambda (_) nil))
                                 (branching-factor 2))
  "Randomly generate a tree with BRANCHING-FACTOR using VALUE-FN to compute the
node values.  VALUE-FN is called with the current-depth of the node.  Useful for
generating test data.  Warning this function can overflow the stack."
  (cl-labels ((do-random
               (d vf bf)
               (make-node
                :value (funcall vf d)
                :children (->> (series/range 0 (number/dec bf))
                               (list/map
                                (lambda (_)
                                  (when (random/boolean?)
                                    (do-random d vf bf))))))))
    (do-random 0 value-fn branching-factor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Predicates
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun tree/instance? (tree)
  "Return t if TREE is a tree struct."
  (node-p tree))

(defun tree/leaf? (node)
  "Return t if NODE has no children."
  (maybe/nil? (node-children node)))

(defun tree/balanced? (n xs)
  "Return t if the tree, XS, is balanced.
A tree is balanced if none of the differences between any two depths of two leaf
  nodes in XS is greater than N."
  (> n (->> xs
            tree/leaf-depths
            set/from-list
            set/count
            number/dec)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst tree/enable-testing? t
  "When t, test suite runs.")

;; TODO: Create set of macros for a proper test suite including:
;; - describe (arbitrarily nestable)
;; - it (arbitrarily nestable)
;; - line numbers for errors
;; - accumulated output for synopsis
;; - do we want describe *and* it? Why not a generic label that works for both?
(when tree/enable-testing?
  (let ((tree-a (tree/node 1
                           (list (tree/node 2
                                            (list (tree/node 5)
                                                  (tree/node 6)))
                                 (tree/node 3
                                            (list (tree/node 7)
                                                  (tree/node 8)))
                                 (tree/node 4
                                            (list (tree/node 9)
                                                  (tree/node 10))))))
        (tree-b (tree/node 1
                           (list (tree/node 2
                                            (list (tree/node 5)
                                                  (tree/node 6)))
                                 (tree/node 3)
                                 (tree/node 4
                                            (list (tree/node 9)
                                                  (tree/node 10)))))))
    ;; instance?
    (prelude/assert (tree/instance? tree-a))
    (prelude/assert (tree/instance? tree-b))
    (prelude/refute (tree/instance? '(1 2 3)))
    (prelude/refute (tree/instance? "oak"))
    ;; balanced?
    (prelude/assert (tree/balanced? 1 tree-a))
    (prelude/refute (tree/balanced? 1 tree-b))
    (message "Tests pass!")))

(provide 'tree)
;;; tree.el ends here
