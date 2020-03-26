

(defalias 'ex! 'evil-ex-define-cmd)

(defun delete-file-and-buffer ()
  "Kill the current buffer and deletes the file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (when filename
      (if (vc-backend filename)
          (vc-delete-file filename)
        (progn
          (delete-file filename)
          (message "Deleted file %s" filename)
          (kill-buffer))))))

;;; Commands defined elsewhere
;;(ex! "al[ign]"      #'+evil:align)
;;(ex! "g[lobal]"     #'+evil:global)

;;; Custom commands
;; Editing
(ex! "@"            #'+evil:macro-on-all-lines)   ; TODO Test me
(ex! "al[ign]"      #'+evil:align)
(ex! "enhtml"       #'+web:encode-html-entities)
(ex! "dehtml"       #'+web:decode-html-entities)
(ex! "mc"           #'+evil:mc)
(ex! "iedit"        #'evil-multiedit-ex-match)
(ex! "na[rrow]"     #'+evil:narrow-buffer)
(ex! "retab"        #'+evil:retab)

(ex! "glog" #'magit-log-buffer-file)

;; External resources
;; TODO (ex! "db"          #'doom:db)
;; TODO (ex! "dbu[se]"     #'doom:db-select)
;; TODO (ex! "go[ogle]"    #'doom:google-search)
(ex! "lo[okup]"    #'+jump:online)
(ex! "dash"        #'+lookup:dash)
(ex! "dd"          #'+lookup:devdocs)
(ex! "http"        #'httpd-start)            ; start http server
(ex! "repl"        #'+eval:repl)             ; invoke or send to repl
;; TODO (ex! "rx"          'doom:regex)             ; open re-builder
(ex! "sh[ell]"     #'+eshell:run)
(ex! "t[mux]"      #'+tmux:run)              ; send to tmux
(ex! "tcd"         #'+tmux:cd-here)          ; cd to default-directory in tmux
(ex! "x"           #'doom/open-project-scratch-buffer)

;; GIT
(ex! "gist"        #'+gist:send)  ; send current buffer/region to gist
(ex! "gistl"       #'+gist:list)  ; list gists by user
(ex! "gbrowse"     #'+vcs/git-browse)        ; show file in github/gitlab
(ex! "gissues"     #'+vcs/git-browse-issues) ; show github issues
(ex! "git"         #'magit-status)           ; open magit status window
(ex! "gstage"      #'magit-stage)
(ex! "gunstage"    #'magit-unstage)
(ex! "gblame"      #'magit-blame)
(ex! "grevert"     #'git-gutter:revert-hunk)

;; Dealing with buffers
(ex! "clean[up]"   #'doom/cleanup-buffers)
(ex! "k[ill]"      #'doom/kill-this-buffer)
(ex! "k[ill]all"   #'+hlissner:kill-all-buffers)
(ex! "k[ill]m"     #'+hlissner:kill-matching-buffers)
(ex! "k[ill]o"     #'doom/kill-other-buffers)
(ex! "l[ast]"      #'doom/popup-restore)
(ex! "m[sg]"       #'view-echo-area-messages)
(ex! "pop[up]"     #'doom/popup-this-buffer)

;; Project navigation
(ex! "a"           #'projectile-toggle-between-implementation-and-test)
(ex! "as"          #'projectile-find-implementation-or-test-other-window)
(ex! "av"          #'projectile-find-implementation-or-test-other-window)
(ex! "cd"          #'+hlissner:cd)
(cond ((featurep! :completion ivy)
       (ex! "ag"       #'+ivy:ag)
       (ex! "agc[wd]"  #'+ivy:ag-cwd)
       (ex! "rg"       #'+ivy:rg)
       (ex! "rgc[wd]"  #'+ivy:rg-cwd)
       (ex! "sw[iper]" #'+ivy:swiper)
       (ex! "todo"     #'+ivy:todo))
      ((featurep! :completion helm)
       (ex! "ag"       #'+helm:ag)
       (ex! "agc[wd]"  #'+helm:ag-cwd)
       (ex! "rg"       #'+helm:rg)
       (ex! "rgc[wd]"  #'+helm:rg-cwd)
       (ex! "sw[oop]"  #'+helm:swoop)
       (ex! "todo"     #'+helm:todo)))

;; Project tools
(ex! "build"       #'+eval/build)
(ex! "debug"       #'+debug/run)
(ex! "er[rors]"    #'flycheck-list-errors)

;; File operations
(ex! "cp"          #'+evil:copy-this-file)
(ex! "mv"          #'+evil:move-this-file)
(ex! "rm"          #'+evil:delete-this-file)

;; Sessions/tabs
(ex! "sclear"      #'+workspace/kill-session)
(ex! "sl[oad]"     #'+workspace:load-session)
(ex! "ss[ave]"     #'+workspace:save-session)
(ex! "tabcl[ose]"  #'+workspace:delete)
(ex! "tabclear"    #'doom/kill-all-buffers)
(ex! "tabl[ast]"   #'+workspace/switch-to-last)
(ex! "tabload"     #'+workspace:load)
(ex! "tabn[ew]"    #'+workspace:new)
(ex! "tabn[ext]"   #'+workspace:switch-next)
(ex! "tabp[rev]"   #'+workspace:switch-previous)
(ex! "tabr[ename]" #'+workspace:rename)
(ex! "tabs"        #'+workspace/display)
(ex! "tabsave"     #'+workspace:save)

(ex! "scr[atch]" #'cider-scratch)

;; Org-mode
(ex! "cap"         #'+org-capture/dwim)

(evil-define-command evil-alembic-revision (args)
  (interactive "<a>")
  (apply
   #'generate-alembic-migration
   (read-string "Message: ")
   (s-split "\\s+" (or args ""))))
(ex! "arev[ision]" #'evil-alembic-revision)

(evil-define-command evil-alembic-upgrade (&optional revision)
  (interactive "<a>")
  (alembic-upgrade (or revision "head")))

(ex! "aup[grade]" #'evil-alembic-upgrade)

(evil-define-command evil-alembic-downgrade (&optional revision)
  (interactive "<a>")
  (alembic-downgrade revision))

(ex! "adown[grade]" #'evil-alembic-downgrade)

(evil-define-command evil-alembic (args)
  (interactive "<a>")
  (run-alembic args))

(ex! "alemb[ic]" #'evil-alembic)

;; Elixir
(add-hook! elixir-mode
  (ex! "AV" #'alchemist-project-toggle-file-and-tests-other-window)
  (ex! "A" #'alchemist-project-toggle-file-and-tests))
