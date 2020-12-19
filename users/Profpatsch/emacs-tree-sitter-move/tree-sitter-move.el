;; this is not an actual cursor, just a node.
;; It’s not super efficient, but cursors can’t be *set* to an arbitrary
;; subnode, because they can’t access the parent otherwise.
;; We’d need a way to reset the cursor and walk down to the node?!
(defvar-local tree-sitter-move--cursor nil
  "the buffer-local cursor used for movement")

(defvar-local tree-sitter-move--debug-overlay nil
  "an overlay used to visually display the region currently marked by the cursor")

;;;;; TODO: should everything use named nodes? Only some things?
;;;;; maybe there should be a pair of functions for everything?
;;;;; For now restrict to named nodes.

(defun tree-sitter-move--setup ()
  ;; TODO
  (progn
    ;; TODO: if tree-sitter-mode fails to load, display a better error
    (tree-sitter-mode t)
    (setq tree-sitter-move--cursor (tsc-root-node tree-sitter-tree))
    (add-variable-watcher
     'tree-sitter-move--cursor
     #'tree-sitter-move--debug-overlay-update)))

(defun tree-sitter-move--debug-overlay-update (sym newval &rest _args)
  "variable-watcher to update the debug overlay when the cursor changes"
  (let ((start (tsc-node-start-position newval))
        (end (tsc-node-end-position newval)))
    (symbol-macrolet ((o tree-sitter-move--debug-overlay))
      (if o
          (move-overlay o start end)
        (setq o (make-overlay start end))
        (overlay-put o 'face 'highlight)
        ))))

(defun tree-sitter-move--debug-overlay-teardown ()
  "Turn of the overlay visibility and delete the overlay object"
  (when tree-sitter-move--debug-overlay
    (delete-overlay tree-sitter-move--debug-overlay)
    (setq tree-sitter-move--debug-overlay nil)))

(defun tree-sitter-move--teardown ()
  (setq tree-sitter-move--cursor nil)
  (tree-sitter-move--debug-overlay-teardown)
  (tree-sitter-mode nil))

;; Get the syntax node the cursor is on.
(defun tsc-get-named-node-at-point ()
  (let ((p (point)))
    (tsc-get-named-descendant-for-position-range
     (tsc-root-node tree-sitter-tree) p p)))

(defun tsc-get-first-named-node-with-siblings-up (node)
  "Returns the first 'upwards' node that has siblings. That includes the current
  node, so if the given node has siblings, it is returned. Returns nil if there
  is no such node until the root"
  (when-let ((has-siblings-p
            (lambda (parent-node)
              (> (tsc-count-named-children parent-node)
                 1)))
           (cur node)
           (parent (tsc-get-parent node)))
      (while (not (funcall has-siblings-p parent))
        (setq cur parent)
        (setq parent (tsc-get-parent cur)))
    cur))

(defun tree-sitter-move--set-cursor-to-node (node)
  (setq tree-sitter-move--cursor node))

(defun tree-sitter-move--set-cursor-to-node-at-point ()
  (tree-sitter-move--set-cursor-to-node (tsc-get-named-node-at-point)))

(defun tree-sitter-move--move-point-to-node (node)
  (set-window-point
    (selected-window)
    (tsc-node-start-position node)))


;; interactive commands (“do what I expect” section)

(defun tree-sitter-move-reset ()
  (interactive)
  (tree-sitter-move--set-cursor-to-node-at-point))

(defun tree-sitter-move-right ()
  (interactive)
  (tree-sitter-move--move-skip-non-sibling-nodes 'tsc-get-next-named-sibling))

(defun tree-sitter-move-left ()
  (interactive)
  (tree-sitter-move--move-skip-non-sibling-nodes 'tsc-get-prev-named-sibling))

(defun tree-sitter-move-up ()
  (interactive)
  (tree-sitter-move--move-skip-non-sibling-nodes 'tsc-get-parent))

;; TODO: does not skip siblings yet, because the skip function only goes up (not down)
(defun tree-sitter-move-down ()
  (interactive)
  (tree-sitter-move--move-if-possible (lambda (n) (tsc-get-nth-named-child n 0))))

(defun tree-sitter-move--move-skip-non-sibling-nodes (move-fn)
  "Moves to the sidewards next sibling. If the current node does not have siblings, go
  upwards until something has siblings and then move to the side (right or left)."
  (tree-sitter-move--move-if-possible
   (lambda (cur)
     (when-let ((with-siblings
                 (tsc-get-first-named-node-with-siblings-up cur)))
       (funcall move-fn with-siblings)))))

(defun tree-sitter-move--move-if-possible (dir-fn)
  (let ((next (funcall dir-fn tree-sitter-move--cursor)))
    (when next
      (tree-sitter-move--set-cursor-to-node next)
      (tree-sitter-move--move-point-to-node next))))

; mostly stolen from tree-sitter-mode
;;;###autoload
(define-minor-mode tree-sitter-move-mode
  "Minor mode to do cursor movements via tree-sitter"
  :init-value nil
  :lighter " tree-sitter-move"
  (if tree-sitter-move-mode
      (tree-sitter--error-protect
          (progn
            (tree-sitter-move--setup))
        (setq tree-sitter-move-mode nil)
        (tree-sitter-move--teardown))
    (lambda ())
    (tree-sitter-move--teardown)))
