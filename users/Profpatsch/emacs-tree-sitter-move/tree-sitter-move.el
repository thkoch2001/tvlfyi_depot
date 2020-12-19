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
(defun tsc-node-named-node-at-point ()
  (let ((p (point)))
    (tsc-get-named-descendant-for-position-range
     (tsc-root-node tree-sitter-tree) p p)))

(defun tsc-get-node-at-point ()
  (let ((p (point)))
    (tsc-get-descendant-for-position-range
     (tsc-root-node tree-sitter-tree) p p)))

(defun tsc-get-first-named-node-with-siblings-up (node)
  "Returns the first 'upwards' node that has siblings. That includes the current
  node, so if the given node has siblings, it is returned."
  (let ((has-siblings-p
         (lambda (n)
           (> (tsc-count-named-children (tsc-get-parent n))
              1)))
        (res node))
    (while (not (funcall has-siblings-p res))
      ;; TODO tsc-get-parent is called twice, nicer somehow?
      (setq res (tsc-get-parent res)))
    res))

(defun tree-sitter-move--set-cursor-to-node (node)
  (setq tree-sitter-move--cursor node))

(defun tree-sitter-move--set-cursor-to-node-at-point ()
  (tree-sitter-move--set-cursor-to-node (tsc-get-node-at-point)))

(defun tree-sitter-move--move-point-to-node (node)
  (set-window-point
    (selected-window)
    (tsc-node-start-position node)))


;; interactive commands (“do what I expect” section)

(defun tree-sitter-move-right ()
  "Moves to the next sibling. If the current node does not have siblings, go
  upwards until something has siblings and then move right."
  (interactive)
  (tree-sitter-move--set-cursor-to-node-at-point)
  (let ((next (tsc-get-next-named-sibling
               (tsc-get-first-named-node-with-siblings-up tree-sitter-move--cursor))))
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
