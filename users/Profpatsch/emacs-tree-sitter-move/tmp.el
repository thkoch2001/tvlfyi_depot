(defun tree-sitter-load-from-grammar-dir (grammar-dir sym lang-name)
  (tree-sitter-load
   sym
   (format "%s/bin/%s"
           (getenv grammar-dir)
           lang-name)))

(defun tree-sitter-init-tmp-langs (alist)
  (mapcar
   (lambda (lang)
     (pcase-let ((`(,name ,sym ,mode) lang))
       (tree-sitter-load-from-grammar-dir "TREE_SITTER_GRAMMAR_DIR" sym name)
       (cons mode sym)))
   alist))


(setq tree-sitter-major-mode-language-alist
      (tree-sitter-init-tmp-langs
       '(("python" python python-mode)
         ("json" json js-mode)
         ("bash" bash sh-mode)
         )))

(define-key evil-normal-state-map (kbd "C-.") #'tree-sitter-move-reset)
(define-key evil-normal-state-map (kbd "C-<right>") #'tree-sitter-move-right)
(define-key evil-normal-state-map (kbd "C-<left>") #'tree-sitter-move-left)
(define-key evil-normal-state-map (kbd "C-<up>") #'tree-sitter-move-up)
(define-key evil-normal-state-map (kbd "C-<down>") #'tree-sitter-move-down)
