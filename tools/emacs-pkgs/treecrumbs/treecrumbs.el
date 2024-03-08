;; treecrumbs.el --- Fast, tree-sitter based breadcrumbs  -*- lexical-binding: t; -*-
;;
;; Copyright (C) Free Software Foundation, Inc.
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; Author: Vincent Ambo <tazjin@tvl.su>
;; Created: 2024-03-08
;; Version: 1.0
;; Keywords: convenience
;; Package-Requires: ((emacs "29.1"))
;; URL: https://code.tvl.fyi/tree/tools/emacs-pkgs/treecrumbs
;;
;; This file is not (yet) part of GNU Emacs.

;;; Commentary:

;; This package provides a tree-sitter based implementation of "breadcrumbs",
;; that is indicators displaying where in the semantic structure of a document
;; the point is currently located.
;;
;; Imagine a large YAML-document where the names of the parent keys are far out
;; of view: Treecrumbs can quickly display the hierarchy of keys (e.g. `foo < []
;; < baz') and help figure out where point is.
;;
;; Treecrumbs only works if a tree-sitter parser for the target language is
;; available in the buffer, and the language is supported in the
;; `treecrumbs-languages'. Adding a new language is not difficult, and patches
;; for this are welcome.
;;
;; To active treecrumbs, enable `treecrumbs-mode'. This buffer-local minor mode
;; adds the crumbs to the buffer's `header-line-format'. Alternatively, users
;; can also use the `treecrumbs-line-segment' either in their own header-line,
;; tab-line or mode-line configuration.

;;; Code:

(require 'treesit)

(defvar treecrumbs-languages
  `(;; In YAML documents, crumbs are generated from the keys of maps,
    ;; and from elements of arrays.
    (yaml . (("block_mapping_pair" .
              ,(treesit-query-compile 'yaml '((block_mapping_pair key: (_) @key))))
             ("block_sequence_item" . "[]"))))

  "Describes the tree-sitter language grammars supported by
treecrumbs, and how the breadcrumbs for their node types are
generated.

Alist of symbols representing tree-sitter languages (e.g. `yaml')
to another alist (the \"node type list\") describing how
different node types should be displayed in the crumbs.

The node type list has string keys (corresponding to tree-sitter
node type names), and its values are either a static
string (which is displayed verbatim in the crumbs if such a node
is encountered), or a tree-sitter query.

Tree-sitter queries are executed on the node, and MUST capture
exactly one argument named `@key', which should be a node whose
text value will become the braedcrumb (e.g. the name of a
function, the key in a map, ...).

Treecrumbs will only consider node types that are mentioned in
the node type list. All other nodes are ignored when constructing
the crumbs.")

(defvar-local treecrumbs--current-crumbs nil
  "Current crumbs to display in the header line. Only updated when
the node under point changes.")

(defun treecrumbs--crumbs-for (node)
  "Construct the crumbs for the given NODE, if its language is
supported in `treecrumbs-languages'. This functions return value
is undefined, it directly updates the buffer-local
`treecrumbs--current-crumbs'."
  (let ((lang (cdr (assoc (treesit-node-language node) treecrumbs-languages))))
    (unless lang
      (user-error "No supported treecrumbs language at point!"))

    (setq-local treecrumbs--current-crumbs "")
    (treesit-parent-while
     node
     (lambda (parent)
       (when-let ((query (cdr (assoc (treesit-node-type parent) lang))))

         (setq-local treecrumbs--current-crumbs
               (concat treecrumbs--current-crumbs
                       (if (string-empty-p treecrumbs--current-crumbs) ""
                         " < ")

                       (if (stringp query)
                           query
                         (substring-no-properties
                          (treesit-node-text (cdar (treesit-query-capture parent query))))))))
       t))))


(defvar-local treecrumbs--last-node nil
  "Caches the node that was last seen at point.")

(defun treecrumbs-at-point ()
  "Returns the treecrumbs at point as a string, if point is on a
node in a language supported in `treecrumbs-languages'.

The last known crumbs in a given buffer are cached, and only if
the node under point changes are they updated."
  (let ((node (treesit-node-at (point))))
    (when (or (not treecrumbs--current-crumbs)
              (not (equal treecrumbs--last-node node)))
      (setq-local treecrumbs--last-node node)
      (treecrumbs--crumbs-for node)))

  treecrumbs--current-crumbs)

(defvar treecrumbs-line-segment
  '(:eval (treecrumbs-at-point))

  "Treecrumbs segment for use in the header-line or mode-line.")

;;;###autoload
(define-minor-mode treecrumbs-mode
  "Display header line hints about current position in structure."
  :init-value nil
  :lighter " Crumbs"
  (if treecrumbs-mode
      (if (treesit-parser-list)
          (push treecrumbs-line-segment header-line-format)
        (user-error "Treecrumbs mode works only in tree-sitter based buffers!"))
    (setq header-line-format
          (delq treecrumbs-line-segment header-line-format))))

(provide 'treecrumbs)
;;; treecrumbs.el ends here
