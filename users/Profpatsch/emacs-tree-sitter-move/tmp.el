(tree-sitter-load
 'python
 (format "%s/bin/python"
         (getenv "TREE_SITTER_GRAMMAR_DIR")))

(setq tree-sitter-major-mode-language-alist
      '((python-mode . python)))


(define-key evil-normal-state-map (kbd "C-.") #'tree-sitter-move-reset)
(define-key evil-normal-state-map (kbd "C-<right>") #'tree-sitter-move-right)
;; (define-key evil-normal-state-map (kbd "C-<left>") 'sp-backward-parallel-sexp)
;; (define-key evil-normal-state-map (kbd "C-<down>") 'sp-down-sexp)
;; (define-key evil-normal-state-map (kbd "C-<up>") 'sp-backward-up-sexp)
