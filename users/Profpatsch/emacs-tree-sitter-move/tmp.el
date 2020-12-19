(tree-sitter-load
 'python
 (format "%s/bin/python"
         (getenv "TREE_SITTER_GRAMMAR_DIR")))

(setq tree-sitter-major-mode-language-alist
      '((python-mode . python)))


(define-key evil-normal-state-map (kbd "C-.") #'tree-sitter-move-reset)
(define-key evil-normal-state-map (kbd "C-<right>") #'tree-sitter-move-right)
(define-key evil-normal-state-map (kbd "C-<left>") #'tree-sitter-move-left)
(define-key evil-normal-state-map (kbd "C-<up>") 'tree-sitter-move-up)
(define-key evil-normal-state-map (kbd "C-<down>") 'tree-sitter-move-down)
