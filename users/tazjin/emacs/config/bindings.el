;; Font size
(define-key global-map (kbd "C-=") 'increase-default-text-scale) ;; '=' because there lies '+'
(define-key global-map (kbd "C--") 'decrease-default-text-scale)
(define-key global-map (kbd "C-x C-0") 'set-default-text-scale)

;; What does <tab> do? Well, it depends ...
(define-key prog-mode-map (kbd "<tab>") #'company-indent-or-complete-common)

;; imenu instead of insert-file
(global-set-key (kbd "C-x i") 'imenu)

;; Window switching. (C-x o goes to the next window)
(windmove-default-keybindings) ;; Shift+direction

;; Start eshell or switch to it if it's active.
(global-set-key (kbd "C-x m") 'eshell)

(global-set-key (kbd "C-x C-p") 'browse-repositories)
(global-set-key (kbd "M-g M-g") 'goto-line-with-feedback)

;; Miscellaneous editing commands
(global-set-key (kbd "C-c w") 'whitespace-cleanup)
(global-set-key (kbd "C-c a") 'align-regexp)
(global-set-key (kbd "C-c m") 'mc/mark-dwim)

;; Browse URLs (very useful for Gitlab's SSH output!)
(global-set-key (kbd "C-c b p") 'browse-url-at-point)
(global-set-key (kbd "C-c b b") 'browse-url)

;; C-x REALLY QUIT (idea by @magnars)
(global-set-key (kbd "C-x r q") 'save-buffers-kill-terminal)
(global-set-key (kbd "C-x C-c") 'ignore)

;; Open a file in project:
(global-set-key (kbd "C-c f") 'project-find-file)

;; Search in a project
(global-set-key (kbd "C-c r g") 'rg-in-project)

;; Open a file via magit:
(global-set-key (kbd "C-c C-f") #'magit-find-file-worktree)

;; Insert TODO comments
(global-set-key (kbd "C-c t") 'insert-todo-comment)

;; Make sharing music easier
(global-set-key (kbd "s-s w") #'songwhip-lookup-url)

;; Open the depot
(global-set-key (kbd "s-s d") #'tvl-depot-status)

;; Open any repo through zoxide
(global-set-key (kbd "s-s r") #'zoxide-open-magit)

;; Add subthread collapsing to notmuch-show.
;;
;; C-, closes a thread, C-. opens a thread. This mirrors stepping
;; in/out of definitions.
(define-key notmuch-show-mode-map (kbd "C-,") 'notmuch-show-open-or-close-subthread)
(define-key notmuch-show-mode-map (kbd "C-.")
  (lambda ()
    (interactive)
    (notmuch-show-open-or-close-subthread t))) ;; open

;; Get rid of the annoying `save-some-buffers' shortcut which I
;; *NEVER* use intentionally.
(unbind-key (kbd "C-x s") 'global-map)

;; German keyboard layout with Y and Z in the correct place.

(quail-define-package
 "german-qwerty" "German" "DE@" t
 "German (Deutsch) input method with QWERTY keys"
 nil t t t t nil nil nil nil nil t)

;; 1!  2"  3§  4$  5%  6&  7/  8(  9)  0=  ß?  [{  ]}
;;  qQ  wW  eE  rR  tT  yY  uU  iI  oO  pP  üÜ  +*
;;   aA  sS  dD  fF  gG  hH  jJ  kK  lL  öÖ  äÄ  #^
;;    zZ  xX  cC  vV  bB  nN  mM  ,;  .:  -_

(quail-define-rules
 ("-" ?ß)
 ("=" ?\[)
 ("`" ?\])
 ("[" ?ü)
 ("]" ?+)
 (";" ?ö)
 ("'" ?ä)
 ("\\" ?#)
 ("/" ?-)

 ("@" ?\")
 ("#" ?§)
 ("^" ?&)
 ("&" ?/)
 ("*" ?\()
 ("(" ?\))
 (")" ?=)
 ("_" ??)
 ("+" ?{)
 ("~" ?})
 ("{" ?Ü)
 ("}" ?*)
 (":" ?Ö)
 ("\"" ?Ä)
 ("|" ?^)
 ("<" ?\;)
 (">" ?:)
 ("?" ?_))

(provide 'bindings)
