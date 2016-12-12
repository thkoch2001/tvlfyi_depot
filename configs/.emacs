(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")
(el-get 'sync)

(load-theme 'monokai t)

;; Exit insert mode by pressing jk in sequence
(setq key-chord-two-keys-delay 0.5)
(key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
(key-chord-mode 1)


(setq
el-get-sources
'((:name evil
  :after (progn
    ;; my vim bindings
    ; (define-key evil-normal-state-map "\C-;" 'comment-line)

    ;; (define-key evil-normal-state-map "vv" 'split-window-vertically)
    ;; (define-key evil-normal-state-map "vs" 'split-window-vertically)
    ;; (define-key evil-normal-state-map "ss" 'split-window-horizontally)
    ;; (define-key evil-normal-state-map "sp" 'split-window-horizontally)

    (define-key evil-normal-state-map "\S-h" 'evil-beginning-of-line)
    (define-key evil-normal-state-map "\S-l" 'evil-end-of-line)
    (define-key evil-visual-state-map "\S-h" 'evil-beginning-of-line)
    (define-key evil-visual-state-map "\S-l" 'evil-beginning-of-line)
    
    (define-key evil-insert-state-map "\C-a" 'beginning-of-line)
    (define-key evil-insert-state-map "\C-e" 'end-of-line)))

(:name auto-complete
  :after (progn
    (define-key ac-complete-mode-map "\C-n" 'ac-next)
    (define-key ac-complete-mode-map "\C-p" 'ac-previous)))))


(setq
my:el-get-packages
'(el-get
  alchemist
  auto-complete
  evil
  monokai-theme
  neotree))

(el-get 'sync my:el-get-packages)

;; Generic Settings
;; Line numbering
(line-number-mode 1)                ; have line numbers and
(column-number-mode 1)              ; column numbers in the mode line
(global-linum-mode 1)               ; add line numbers on the left

;; activates evil-mode
(evil-mode 1)

;; evil-leader settings
;; enables evil-leader every time evil-mode is loaded.
(global-evil-leader-mode)

;; change the <leader> key
(evil-leader/set-leader "<SPC>")


;; neotree settings
(evil-leader/set-key
  "n" 'neotree-toggle
  )

