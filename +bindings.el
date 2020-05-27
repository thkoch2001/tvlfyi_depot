;; /+bindings.el -*- lexical-binding: t; -*-

(load! "utils")
(require 'f)
(require 'predd)

(defmacro find-file-in! (path &optional project-p)
  "Returns an interactive function for searching files."
  `(lambda () (interactive)
     (let ((default-directory ,path))
       (call-interactively
        ',(command-remapping
           (if project-p
               #'projectile-find-file
             #'find-file))))))

(defun dired-mode-p () (eq 'dired-mode major-mode))

(defun grfn/dired-minus ()
  (interactive)
  (if (dired-mode-p)
      (dired-up-directory)
    (when buffer-file-name
      (-> (buffer-file-name)
          (f-dirname)
          (dired)))))

(defmacro define-move-and-insert
    (name &rest body)
  `(defun ,name (count &optional vcount skip-empty-lines)
     ;; Following interactive form taken from the source for `evil-insert'
     (interactive
      (list (prefix-numeric-value current-prefix-arg)
            (and (evil-visual-state-p)
                 (memq (evil-visual-type) '(line block))
                 (save-excursion
                   (let ((m (mark)))
                     ;; go to upper-left corner temporarily so
                     ;; `count-lines' yields accurate results
                     (evil-visual-rotate 'upper-left)
                     (prog1 (count-lines evil-visual-beginning evil-visual-end)
                       (set-mark m)))))
            (evil-visual-state-p)))
     (atomic-change-group
       ,@body
       (evil-insert count vcount skip-empty-lines))))

(define-move-and-insert grfn/insert-at-sexp-end
  (when (not (equal (get-char) "("))
    (backward-up-list))
  (forward-sexp)
  (backward-char))

(define-move-and-insert grfn/insert-at-sexp-start
  (backward-up-list)
  (forward-char))

(define-move-and-insert grfn/insert-at-form-start
  (backward-sexp)
  (backward-char)
  (insert " "))

(define-move-and-insert grfn/insert-at-form-end
  (forward-sexp)
  (insert " "))

(load! "splitjoin")

(defun +hlissner/install-snippets ()
  "Install my snippets from https://github.com/hlissner/emacs-snippets into
private/hlissner/snippets."
  (interactive)
  (doom-fetch :github "hlissner/emacs-snippets"
              (expand-file-name "snippets" (doom-module-path :private 'hlissner))))

(defun +hlissner/yank-buffer-filename ()
  "Copy the current buffer's path to the kill ring."
  (interactive)
  (if-let* ((filename (or buffer-file-name (bound-and-true-p list-buffers-directory))))
      (message (kill-new (abbreviate-file-name filename)))
    (error "Couldn't find filename in current buffer")))

(defmacro +def-finder! (name dir)
  "Define a pair of find-file and browse functions."
  `(progn
     (defun ,(intern (format "+find-in-%s" name)) ()
       (interactive)
       (let ((default-directory ,dir)
             projectile-project-name
             projectile-require-project-root
             projectile-cached-buffer-file-name
             projectile-cached-project-root)
         (call-interactively #'projectile-find-file)))
     (defun ,(intern (format "+hlissner/browse-%s" name)) ()
       (interactive)
       (let ((default-directory ,dir))
         (call-interactively (command-remapping #'find-file))))))

(+def-finder! templates +file-templates-dir)
(+def-finder! snippets +grfn-snippets-dir)
(+def-finder! dotfiles (expand-file-name ".dotfiles" "~"))
(+def-finder! doomd (expand-file-name ".doom.d" "~"))
(+def-finder! notes +org-dir)
(+def-finder! home-config (expand-file-name "code/system/home" "~"))
(+def-finder! system-config (expand-file-name "code/system/system" "~"))

(defun +grfn/paxedit-kill (&optional n)
  (interactive "p")
  (or (paxedit-comment-kill)
      (when (paxedit-symbol-cursor-within?)
        (paxedit-symbol-kill))
      (paxedit-implicit-sexp-kill n)
      (paxedit-sexp-kill n)
      (message paxedit-message-kill)))
;;;

(map!
 [remap evil-jump-to-tag] #'projectile-find-tag
 [remap find-tag]         #'projectile-find-tag
 ;; ensure there are no conflicts
 :nmvo doom-leader-key nil
 :nmvo doom-localleader-key nil)

(map!
 ;; --- Global keybindings ---------------------------
 ;; Make M-x available everywhere
 :gnvime "M-x" #'execute-extended-command
 :gnvime "A-x" #'execute-extended-command
 ;; Emacs debug utilities
 :gnvime "M-;" #'eval-expression
 :gnvime "M-:" #'doom/open-scratch-buffer
 ;; Text-scaling
 "M-+"       (λ! (text-scale-set 0))
 "M-="       #'text-scale-increase
 "M--"       #'text-scale-decrease
 ;; Simple window navigation/manipulation
 "C-`"       #'doom/popup-toggle
 "C-~"       #'doom/popup-raise
 "M-t"       #'+workspace/new
 "M-T"       #'+workspace/display
 "M-w"       #'delete-window
 "M-W"       #'+workspace/close-workspace-or-frame
 "M-n"       #'evil-buffer-new
 "M-N"       #'make-frame
 "M-1"       (λ! (+workspace/switch-to 0))
 "M-2"       (λ! (+workspace/switch-to 1))
 "M-3"       (λ! (+workspace/switch-to 2))
 "M-4"       (λ! (+workspace/switch-to 3))
 "M-5"       (λ! (+workspace/switch-to 4))
 "M-6"       (λ! (+workspace/switch-to 5))
 "M-7"       (λ! (+workspace/switch-to 6))
 "M-8"       (λ! (+workspace/switch-to 7))
 "M-9"       (λ! (+workspace/switch-to 8))
 "M-0"       #'+workspace/switch-to-last
 ;; Other sensible, textmate-esque global bindings
 :ne "M-r"   #'+eval/buffer
 :ne "M-R"   #'+eval/region-and-replace
 :ne "M-b"   #'+eval/build
 :ne "M-a"   #'mark-whole-buffer
 :ne "M-c"   #'evil-yank
 :ne "M-q"   (if (daemonp) #'delete-frame #'save-buffers-kill-emacs)
 :ne "M-f"   #'swiper
 :ne "C-M-f" #'doom/toggle-fullscreen
 :n  "M-s"   #'save-buffer
 :m  "A-j"   #'+hlissner:multi-next-line
 :m  "A-k"   #'+hlissner:multi-previous-line
 :nv "C-SPC" #'+evil:fold-toggle
 :gnvimer "M-v" #'clipboard-yank
 ;; Easier window navigation
 :en "C-h"   #'evil-window-left
 :en "C-j"   #'evil-window-down
 :en "C-k"   #'evil-window-up
 :en "C-l"   #'evil-window-right
 :n "U" #'undo-tree-visualize

 "C-x p"     #'doom/other-popup

 :n "K" #'+lookup/documentation
 :n "g d" #'+lookup/definition


 ;; --- <leader> -------------------------------------
 (:leader
   :desc "Ex command"              :nv ";"  #'evil-ex
   :desc "M-x"                     :nv ":"  #'execute-extended-command
   :desc "Pop up scratch buffer"   :nv "x"  #'doom/open-scratch-buffer
   :desc "Org Capture"             :nv "X"  #'org-capture
   :desc "Org Capture"             :nv "a"  #'org-capture

   ;; Most commonly used
   :desc "Find file in project"    :n "SPC" #'projectile-find-file
   :desc "Switch workspace buffer" :n ","   #'persp-switch-to-buffer
   :desc "Switch buffer"           :n "<"   #'switch-to-buffer
   :desc "Browse files"            :n "."   #'find-file
   :desc "Toggle last popup"       :n "~"   #'doom/popup-toggle
   :desc "Eval expression"         :n "`"   #'eval-expression
   :desc "Blink cursor line"       :n "DEL" #'+doom/blink-cursor
   :desc "Jump to bookmark"        :n "RET" #'bookmark-jump

   ;; C-u is used by evil
   :desc "Universal argument"      :n "u"  #'universal-argument
   :desc "window"                  :n "w"  evil-window-map

   (:desc "previous..." :prefix "["
     :desc "Text size"             :nv "[" #'text-scale-decrease
     :desc "Buffer"                :nv "b" #'doom/previous-buffer
     :desc "Diff Hunk"             :nv "d" #'git-gutter:previous-hunk
     :desc "Todo"                  :nv "t" #'hl-todo-previous
     :desc "Error"                 :nv "e" #'previous-error
     :desc "Workspace"             :nv "w" #'+workspace/switch-left
     :desc "Smart jump"            :nv "h" #'smart-backward
     :desc "Spelling error"        :nv "s" #'evil-prev-flyspell-error
     :desc "Spelling correction"   :n  "S" #'flyspell-correct-previous-word-generic
     :desc "Git conflict"          :n  "n" #'smerge-prev)

   (:desc "next..." :prefix "]"
     :desc "Text size"             :nv "]" #'text-scale-increase
     :desc "Buffer"                :nv "b" #'doom/next-buffer
     :desc "Diff Hunk"             :nv "d" #'git-gutter:next-hunk
     :desc "Todo"                  :nv "t" #'hl-todo-next
     :desc "Error"                 :nv "e" #'next-error
     :desc "Workspace"             :nv "w" #'+workspace/switch-right
     :desc "Smart jump"            :nv "l" #'smart-forward
     :desc "Spelling error"        :nv "s" #'evil-next-flyspell-error
     :desc "Spelling correction"   :n  "S" #'flyspell-correct-word-generic
     :desc "Git conflict"          :n  "n" #'smerge-next)

   (:desc "search" :prefix "/"
     :desc "Swiper"                :nv "/" #'swiper
     :desc "Imenu"                 :nv "i" #'imenu
     :desc "Imenu across buffers"  :nv "I" #'imenu-anywhere
     :desc "Online providers"      :nv "o" #'+lookup/online-select)

   (:desc "workspace" :prefix "TAB"
     :desc "Display tab bar"          :n "TAB" #'+workspace/display
     :desc "New workspace"            :n "n"   #'+workspace/new
     :desc "Load workspace from file" :n "l"   #'+workspace/load
     :desc "Load last session"        :n "L"   (λ! (+workspace/load-session))
     :desc "Save workspace to file"   :n "s"   #'+workspace/save
     :desc "Autosave current session" :n "S"   #'+workspace/save-session
     :desc "Switch workspace"         :n "."   #'+workspace/switch-to
     :desc "Kill all buffers"         :n "x"   #'doom/kill-all-buffers
     :desc "Delete session"           :n "X"   #'+workspace/kill-session
     :desc "Delete this workspace"    :n "d"   #'+workspace/delete
     :desc "Load session"             :n "L"   #'+workspace/load-session
     :desc "Next workspace"           :n "]"   #'+workspace/switch-right
     :desc "Previous workspace"       :n "["   #'+workspace/switch-left
     :desc "Switch to 1st workspace"  :n "1"   (λ! (+workspace/switch-to 0))
     :desc "Switch to 2nd workspace"  :n "2"   (λ! (+workspace/switch-to 1))
     :desc "Switch to 3rd workspace"  :n "3"   (λ! (+workspace/switch-to 2))
     :desc "Switch to 4th workspace"  :n "4"   (λ! (+workspace/switch-to 3))
     :desc "Switch to 5th workspace"  :n "5"   (λ! (+workspace/switch-to 4))
     :desc "Switch to 6th workspace"  :n "6"   (λ! (+workspace/switch-to 5))
     :desc "Switch to 7th workspace"  :n "7"   (λ! (+workspace/switch-to 6))
     :desc "Switch to 8th workspace"  :n "8"   (λ! (+workspace/switch-to 7))
     :desc "Switch to 9th workspace"  :n "9"   (λ! (+workspace/switch-to 8))
     :desc "Switch to last workspace" :n "0"   #'+workspace/switch-to-last)

   (:desc "buffer" :prefix "b"
     :desc "New empty buffer"        :n "n" #'evil-buffer-new
     :desc "Switch workspace buffer" :n "b" #'persp-switch-to-buffer
     :desc "Switch buffer"           :n "B" #'switch-to-buffer
     :desc "Kill buffer"             :n "k" #'doom/kill-this-buffer
     :desc "Kill other buffers"      :n "o" #'doom/kill-other-buffers
     :desc "Save buffer"             :n "s" #'save-buffer
     :desc "Pop scratch buffer"      :n "x" #'doom/open-scratch-buffer
     :desc "Bury buffer"             :n "z" #'bury-buffer
     :desc "Next buffer"             :n "]" #'doom/next-buffer
     :desc "Previous buffer"         :n "[" #'doom/previous-buffer
     :desc "Sudo edit this file"     :n "S" #'doom/sudo-this-file)

   (:desc "code" :prefix "c"
     :desc "List errors"               :n  "x" #'flycheck-list-errors
     :desc "Evaluate buffer/region"    :n  "e" #'+eval/buffer
                                       :v  "e" #'+eval/region
     :desc "Evaluate & replace region" :nv "E" #'+eval:replace-region
     :desc "Build tasks"               :nv "b" #'+eval/build
     :desc "Jump to definition"        :n  "d" #'+lookup/definition
     :desc "Jump to references"        :n  "D" #'+lookup/references
     :desc "Open REPL"                 :n  "r" #'+eval/open-repl
                                       :v  "r" #'+eval:repl)

   (:desc "file" :prefix "f"
     :desc "Find file"                  :n "." #'find-file
     :desc "Sudo find file"             :n ">" #'doom/sudo-find-file
     :desc "Find file in project"       :n "/" #'projectile-find-file
     :desc "Find file from here"        :n "?" #'counsel-file-jump
     :desc "Find other file"            :n "a" #'projectile-find-other-file
     :desc "Open project editorconfig"  :n "c" #'editorconfig-find-current-editorconfig
     :desc "Find file in dotfiles"      :n "d" #'+find-in-dotfiles
     :desc "Find file in system config" :n "s" #'+find-in-system-config
     :desc "Find file in home config"   :n "h" #'+find-in-home-config
     :desc "Browse dotfiles"            :n "D" #'+hlissner/browse-dotfiles
     :desc "Find file in emacs.d"       :n "e" #'+find-in-doomd
     :desc "Browse emacs.d"             :n "E" #'+hlissner/browse-doomd
     :desc "Recent files"               :n "r" #'recentf-open-files
     :desc "Recent project files"       :n "R" #'projectile-recentf
     :desc "Yank filename"              :n "y" #'+hlissner/yank-buffer-filename)

   (:desc "git" :prefix "g"
     :desc "Git status"            :n  "S" #'magit-status
     :desc "Git blame"             :n  "b" #'magit-blame
     :desc "Git time machine"      :n  "t" #'git-timemachine-toggle
     :desc "Git stage hunk"        :n  "s" #'git-gutter:stage-hunk
     :desc "Git revert hunk"       :n  "r" #'git-gutter:revert-hunk
     :desc "Git revert buffer"     :n  "R" #'vc-revert
     ;; :desc "List gists"            :n  "g" #'+gist:list
     :desc "Git grep"              :n  "g" #'counsel-git-grep
     :desc "Checkout Branch"       :n  "c" #'counsel-git-checkout
     :desc "Next hunk"             :nv "]" #'git-gutter:next-hunk
     :desc "Previous hunk"         :nv "[" #'git-gutter:previous-hunk

     (:desc "smerge" :prefix "m"
       :desc "Keep Current" :n "SPC" #'smerge-keep-current
       :desc "Keep All"     :n "a" #'smerge-keep-all))

   (:desc "help" :prefix "h"
     :n "h" help-map
     :desc "Apropos"               :n  "a" #'apropos
     :desc "Reload theme"          :n  "R" #'doom//reload-theme
     :desc "Find library"          :n  "l" #'find-library
     :desc "Toggle Emacs log"      :n  "m" #'doom/popup-toggle-messages
     :desc "Command log"           :n  "L" #'global-command-log-mode
     :desc "Describe function"     :n  "f" #'describe-function
     :desc "Describe key"          :n  "k" #'describe-key
     :desc "Describe char"         :n  "c" #'describe-char
     :desc "Describe mode"         :n  "M" #'describe-mode
     :desc "Describe variable"     :n  "v" #'describe-variable
     :desc "Describe face"         :n  "F" #'describe-face
     :desc "Describe DOOM setting" :n  "s" #'doom/describe-setting
     :desc "Describe DOOM module"  :n  "d" #'doom/describe-module
     :desc "Find definition"       :n  "." #'+lookup/definition
     :desc "Find references"       :n  "/" #'+lookup/references
     :desc "Find documentation"    :n  "h" #'+lookup/documentation
     :desc "What face"             :n  "'" #'doom/what-face
     :desc "What minor modes"      :n  ";" #'doom/what-minor-mode
     :desc "Info"                  :n  "i" #'info
     :desc "Toggle profiler"       :n  "p" #'doom/toggle-profiler)

   (:desc "insert" :prefix "i"
     :desc "From kill-ring"        :nv "y" #'counsel-yank-pop
     :desc "From snippet"          :nv "s" #'yas-insert-snippet)

   (:desc "notes" :prefix "n"
     :desc "Agenda"                 :n  "a" #'org-agenda
     :desc "Find file in notes"     :n  "n" #'+find-in-notes
     :desc "Store link"             :n  "l" #'org-store-link
     :desc "Browse notes"           :n  "N" #'+hlissner/browse-notes
     :desc "Org capture"            :n  "x" #'+org-capture/open
     :desc "Create clubhouse story" :n  "c" #'org-clubhouse-create-story
     :desc "Archive subtree"        :n  "k" #'org-archive-subtree
     :desc "Goto clocked-in note"   :n  "g" #'org-clock-goto
     :desc "Clock Out"              :n  "o" #'org-clock-out)


   (:desc "open" :prefix "o"
     :desc "Default browser"       :n  "b" #'browse-url-of-file
     :desc "Debugger"              :n  "d" #'+debug/open
     :desc "REPL"                  :n  "r" #'+eval/open-repl
     :desc "Terminal"              :n  "t" #'+term/open-popup
     :desc "Terminal in project"   :n  "T" #'+term/open-popup-in-project

     :desc "Slack IM"              :n  "i" #'slack-im-select
     :desc "Slack Channel"         :n  "c" #'slack-channel-select
     :desc "Slack Group"           :n  "g" #'slack-group-select
     :desc "Slack Unreads"         :n  "u" #'slack-select-unread-rooms

     :desc "Email"                 :n "m" #'notmuch-jump-search

     (:desc "ERC" :prefix "e"
       :desc "Channel" :n "c" #'erc-switch-to-buffer)

     ;; applications
     :desc "APP: elfeed"           :n "E" #'=rss
     :desc "APP: twitter"          :n "T" #'=twitter

     (:desc "spotify" :prefix "s"
       :desc "Search track"  :n "t" #'counsel-spotify-search-track
       :desc "Search album"  :n "a" #'counsel-spotify-search-album
       :desc "Search artist" :n "A" #'counsel-spotify-search-artist)

     ;; macos
     (:when IS-MAC
       :desc "Reveal in Finder"          :n "o" #'+macos/reveal-in-finder
       :desc "Reveal project in Finder"  :n "O" #'+macos/reveal-project-in-finder
       :desc "Send to Transmit"          :n "u" #'+macos/send-to-transmit
       :desc "Send project to Transmit"  :n "U" #'+macos/send-project-to-transmit
       :desc "Send to Launchbar"         :n "l" #'+macos/send-to-launchbar
       :desc "Send project to Launchbar" :n "L" #'+macos/send-project-to-launchbar))

   (:desc "Email" :prefix "M"
     :desc "Compose" :n "m" #'mu4e-compose-new
     :desc "Update"  :n "u" #'mu4e-update-mail-and-index
     :desc "Sync"    :n "s" #'mu4e-update-mail-and-index
     :desc "Open"    :n "o" #'mu4e)

   (:desc "project" :prefix "p"
     :desc "Browse project"          :n  "." (find-file-in! (doom-project-root))
     :desc "Find file in project"    :n  "/" #'projectile-find-file
     :desc "Run cmd in project root" :nv "!" #'projectile-run-shell-command-in-root
     :desc "Switch project"          :n  "p" #'projectile-switch-project
     :desc "Recent project files"    :n  "r" #'projectile-recentf
     :desc "List project tasks"      :n  "t" #'+ivy/tasks
     :desc "Pop term in project"     :n  "o" #'+term/open-popup-in-project
     :desc "Invalidate cache"        :n  "x" #'projectile-invalidate-cache)

   (:desc "quit" :prefix "q"
     :desc "Quit"                   :n "q" #'evil-save-and-quit
     :desc "Quit (forget session)"  :n "Q" #'+workspace/kill-session-and-quit)

   (:desc "remote" :prefix "r"
     :desc "Upload local"           :n "u" #'+upload/local
     :desc "Upload local (force)"   :n "U" (λ! (+upload/local t))
     :desc "Download remote"        :n "d" #'+upload/remote-download
     :desc "Diff local & remote"    :n "D" #'+upload/diff
     :desc "Browse remote files"    :n "." #'+upload/browse
     :desc "Detect remote changes"  :n ">" #'+upload/check-remote)

   (:desc "snippets" :prefix "s"
     :desc "New snippet"            :n  "n" #'yas-new-snippet
     :desc "Insert snippet"         :nv "i" #'yas-insert-snippet
     :desc "Find snippet for mode"  :n  "s" #'yas-visit-snippet-file
     :desc "Find snippet"           :n  "S" #'+find-in-snippets)

   (:desc "toggle" :prefix "t"
     :desc "Flyspell"               :n "s" #'flyspell-mode
     :desc "Flycheck"               :n "f" #'flycheck-mode
     :desc "Line numbers"           :n "l" #'doom/toggle-line-numbers
     :desc "Fullscreen"             :n "f" #'doom/toggle-fullscreen
     :desc "Indent guides"          :n "i" #'highlight-indentation-mode
     :desc "Indent guides (column)" :n "I" #'highlight-indentation-current-column-mode
     :desc "Impatient mode"         :n "h" #'+impatient-mode/toggle
     :desc "Big mode"               :n "b" #'doom-big-font-mode
     :desc "Evil goggles"           :n "g" #'+evil-goggles/toggle))


 ;; --- vim-vinegar
 :n "-" #'grfn/dired-minus
 (:after dired-mode
         (:map dired-mode-map
        "-" #'grfn/dired-minus))

 (:map smartparens-mode-map
   :n "g o" #'sp-raise-sexp)

 ;; --- vim-sexp-mappings-for-regular-people
 (:after paxedit
   (:map paxedit-mode-map
     :i ";"                          #'paxedit-insert-semicolon
     :i "("                          #'paxedit-open-round
     :i "["                          #'paxedit-open-bracket
     :i "{"                          #'paxedit-open-curly
     :n [remap evil-yank-line]       #'paxedit-copy
     :n [remap evil-delete-line]     #'+grfn/paxedit-kill
     :n "g o"                        #'paxedit-sexp-raise
     :n [remap evil-join-whitespace] #'paxedit-compress
     :n "g S"                        #'paxedit-format-1
     :n "g k"                        #'paxedit-backward-up
     :n "g j"                        #'paxedit-backward-end))

 ;; --- vim-splitjoin
 :n [remap evil-join-whitespace] #'+splitjoin/join
 :n "gS"                         #'+splitjoin/split

 ;; --- Personal vim-esque bindings ------------------
 :n  "zx" #'doom/kill-this-buffer
 :n  "ZX" #'bury-buffer
 :n  "]b" #'doom/next-buffer
 :n  "[b" #'doom/previous-buffer
 :n  "]w" #'+workspace/switch-right
 :n  "[w" #'+workspace/switch-left
 :m  "gt" #'+workspace/switch-right
 :m  "gT" #'+workspace/switch-left
 :m  "gd" #'+lookup/definition
 :m  "gD" #'+lookup/references
 :m  "K" #'+lookup/documentation
 :n  "gp" #'+evil/reselect-paste
 :n  "gr" #'+eval:region
 :n  "gR" #'+eval/buffer
 :v  "gR" #'+eval:replace-region
 :v  "@"  #'+evil:macro-on-all-lines
 :n  "g@" #'+evil:macro-on-all-lines
 ;; repeat in visual mode (FIXME buggy)
 :v  "."  #'evil-repeat
 ;; don't leave visual mode after shifting
 :v  "<"  #'+evil/visual-dedent  ; vnoremap < <gv
 :v  ">"  #'+evil/visual-indent  ; vnoremap > >gv
 ;; paste from recent yank register (which isn't overwritten)
 :v  "C-p" "\"0p"

 (:map evil-window-map ; prefix "C-w"
   ;; Navigation
   "C-h"     #'evil-window-left
   "C-j"     #'evil-window-down
   "C-k"     #'evil-window-up
   "C-l"     #'evil-window-right
   "C-w"     #'ace-window
   ;; Swapping windows
   "H"       #'+evil/window-move-left
   "J"       #'+evil/window-move-down
   "K"       #'+evil/window-move-up
   "L"       #'+evil/window-move-right
   "C-S-w"   #'ace-swap-window
   ;; Window undo/redo
   "u"       #'winner-undo
   "C-u"     #'winner-undo
   "C-r"     #'winner-redo
   "o"       #'doom/window-enlargen
   ;; Delete window
   "c"       #'+workspace/close-window-or-workspace
   "C-C"     #'ace-delete-window
   ;; Popups
   "p"       #'doom/popup-toggle
   "m"       #'doom/popup-toggle-messages
   "P"       #'doom/popup-close-all)


 ;; --- Plugin bindings ------------------------------
 ;; auto-yasnippet
 :i  [C-tab] #'aya-expand
 :nv [C-tab] #'aya-create

 ;; company-mode (vim-like omnicompletion)
 :i "C-SPC"  #'+company/complete
 (:prefix "C-x"
   :i "C-l"   #'+company/whole-lines
   :i "C-k"   #'+company/dict-or-keywords
   :i "C-f"   #'company-files
   :i "C-]"   #'company-etags
   :i "s"     #'company-ispell
   :i "C-s"   #'company-yasnippet
   :i "C-o"   #'company-capf
   :i "C-n"   #'company-dabbrev-code
   :i "C-p"   #'+company/dabbrev-code-previous)
 (:after company
   (:map company-active-map
     ;; Don't interfere with `evil-delete-backward-word' in insert mode
     "C-w"        nil
     "C-o"        #'company-search-kill-others
     "C-n"        #'company-select-next
     "C-p"        #'company-select-previous
     "C-h"        #'company-quickhelp-manual-begin
     "C-S-h"      #'company-show-doc-buffer
     "C-S-s"      #'company-search-candidates
     "C-s"        #'company-filter-candidates
     "C-SPC"      #'company-complete-common
     "C-h"        #'company-quickhelp-manual-begin
     [tab]        #'company-complete-common-or-cycle
     [backtab]    #'company-select-previous
     [escape]     (λ! (company-abort) (evil-normal-state 1)))
   ;; Automatically applies to `company-filter-map'
   (:map company-search-map
     "C-n"        #'company-search-repeat-forward
     "C-p"        #'company-search-repeat-backward
     "C-s"        (λ! (company-search-abort) (company-filter-candidates))
     [escape]     #'company-search-abort))

 ;; counsel
;  (:after counsel
;    (:map counsel-ag-map
;      [backtab]  #'+ivy/wgrep-occur      ; search/replace on results
;      "C-SPC"    #'ivy-call-and-recenter ; preview))

 ;; evil-commentary
 ;; :n  "gc"  #'evil-commentary

 ;; evil-exchange
 :n  "gx"  #'evil-exchange

 ;; evil-magit
 (:after evil-magit
   :map (magit-status-mode-map magit-revision-mode-map)
   :n "C-j" nil
   :n "C-k" nil)

 ;; Smerge
 :n "]n" #'smerge-next
 :n "[n" #'smerge-prev

 ;; evil-mc
 (:prefix "gz"
   :nv "m" #'evil-mc-make-all-cursors
   :nv "u" #'evil-mc-undo-all-cursors
   :nv "z" #'+evil/mc-make-cursor-here
   :nv "t" #'+evil/mc-toggle-cursors
   :nv "n" #'evil-mc-make-and-goto-next-cursor
   :nv "p" #'evil-mc-make-and-goto-prev-cursor
   :nv "N" #'evil-mc-make-and-goto-last-cursor
   :nv "P" #'evil-mc-make-and-goto-first-cursor
   :nv "d" #'evil-mc-make-and-goto-next-match
   :nv "D" #'evil-mc-make-and-goto-prev-match)
 (:after evil-mc
   :map evil-mc-key-map
   :nv "C-n" #'evil-mc-make-and-goto-next-cursor
   :nv "C-N" #'evil-mc-make-and-goto-last-cursor
   :nv "C-p" #'evil-mc-make-and-goto-prev-cursor
   :nv "C-P" #'evil-mc-make-and-goto-first-cursor)

 ;; evil-multiedit
 :v  "R"     #'evil-multiedit-match-all
 :n  "M-d"   #'evil-multiedit-match-symbol-and-next
 :n  "M-D"   #'evil-multiedit-match-symbol-and-prev
 :v  "M-d"   #'evil-multiedit-match-and-next
 :v  "M-D"   #'evil-multiedit-match-and-prev
 :nv "C-M-d" #'evil-multiedit-restore
 (:after evil-multiedit
   (:map evil-multiedit-state-map
     "M-d" #'evil-multiedit-match-and-next
     "M-D" #'evil-multiedit-match-and-prev
     "RET" #'evil-multiedit-toggle-or-restrict-region)
   (:map (evil-multiedit-state-map evil-multiedit-insert-state-map)
     "C-n" #'evil-multiedit-next
     "C-p" #'evil-multiedit-prev))

 ;; evil-snipe
 (:after evil-snipe
   ;; Binding to switch to evil-easymotion/avy after a snipe
   :map evil-snipe-parent-transient-map
   "C-;" (λ! (require 'evil-easymotion)
             (call-interactively
              (evilem-create #'evil-snipe-repeat
                             :bind ((evil-snipe-scope 'whole-buffer)
                                    (evil-snipe-enable-highlight)
                                    (evil-snipe-enable-incremental-highlight))))))

 ;; evil-surround
 :v  "S"  #'evil-surround-region
 :o  "s"  #'evil-surround-edit
 :o  "S"  #'evil-Surround-edit

 ;; expand-region
 :v  "v"  #'er/expand-region
 :v  "V"  #'er/contract-region

 ;; flycheck
 :m  "]e" #'next-error
 :m  "[e" #'previous-error
 (:after flycheck
   :map flycheck-error-list-mode-map
   :n "C-n" #'flycheck-error-list-next-error
   :n "C-p" #'flycheck-error-list-previous-error
   :n "j"   #'flycheck-error-list-next-error
   :n "k"   #'flycheck-error-list-previous-error
   :n "RET" #'flycheck-error-list-goto-error)

 ;; flyspell
 :m  "]S" #'flyspell-correct-word-generic
 :m  "[S" #'flyspell-correct-previous-word-generic

 ;; git-gutter
 :m  "]d" #'git-gutter:next-hunk
 :m  "[d" #'git-gutter:previous-hunk

 ;; git-timemachine
 (:after git-timemachine
   (:map git-timemachine-mode-map
     :n "C-p" #'git-timemachine-show-previous-revision
     :n "C-n" #'git-timemachine-show-next-revision
     :n "[["  #'git-timemachine-show-previous-revision
     :n "]]"  #'git-timemachine-show-next-revision
     :n "q"   #'git-timemachine-quit
     :n "gb"  #'git-timemachine-blame))

 ;; gist
 (:after gist
   :map gist-list-menu-mode-map
   :n "RET" #'+gist/open-current
   :n "b"   #'gist-browse-current-url
   :n "c"   #'gist-add-buffer
   :n "d"   #'gist-kill-current
   :n "f"   #'gist-fork
   :n "q"   #'quit-window
   :n "r"   #'gist-list-reload
   :n "s"   #'gist-star
   :n "S"   #'gist-unstar
   :n "y"   #'gist-print-current-url)

 ;; helm
 (:after helm
   (:map helm-map
     "ESC"        nil
     "C-S-n"      #'helm-next-source
     "C-S-p"      #'helm-previous-source
     "C-u"        #'helm-delete-minibuffer-contents
     "C-w"        #'backward-kill-word
     "C-r"        #'evil-paste-from-register ; Evil registers in helm! Glorious!
     "C-b"        #'backward-word
     [left]       #'backward-char
     [right]      #'forward-char
     [escape]     #'helm-keyboard-quit
     [tab]        #'helm-execute-persistent-action)

   (:after helm-files
     (:map helm-generic-files-map
       :e "ESC"     #'helm-keyboard-quit)
     (:map helm-find-files-map
       "C-w" #'helm-find-files-up-one-level
       "TAB" #'helm-execute-persistent-action))

   (:after helm-ag
     (:map helm-ag-map
       "<backtab>"  #'helm-ag-edit)))

 ;; hl-todo
 :m  "]t" #'hl-todo-next
 :m  "[t" #'hl-todo-previous

 ;; ivy
 (:after ivy
   :map ivy-minibuffer-map
   [escape] #'keyboard-escape-quit
   "C-SPC" #'ivy-call-and-recenter
   "TAB" #'ivy-partial
   "M-v" #'yank
   "M-z" #'undo
   "C-r" #'evil-paste-from-register
   "C-k" #'ivy-previous-line
   "C-j" #'ivy-next-line
   "C-l" #'ivy-alt-done
   "C-w" #'ivy-backward-kill-word
   "C-u" #'ivy-kill-line
   "C-b" #'backward-word
   "C-f" #'forward-word)

 ;; neotree
 (:after neotree
   :map neotree-mode-map
   :n "g"         nil
   :n [tab]       #'neotree-quick-look
   :n "RET"       #'neotree-enter
   :n [backspace] #'evil-window-prev
   :n "c"         #'neotree-create-node
   :n "r"         #'neotree-rename-node
   :n "d"         #'neotree-delete-node
   :n "j"         #'neotree-next-line
   :n "k"         #'neotree-previous-line
   :n "n"         #'neotree-next-line
   :n "p"         #'neotree-previous-line
   :n "h"         #'+neotree/collapse-or-up
   :n "l"         #'+neotree/expand-or-open
   :n "J"         #'neotree-select-next-sibling-node
   :n "K"         #'neotree-select-previous-sibling-node
   :n "H"         #'neotree-select-up-node
   :n "L"         #'neotree-select-down-node
   :n "G"         #'evil-goto-line
   :n "gg"        #'evil-goto-first-line
   :n "v"         #'neotree-enter-vertical-split
   :n "s"         #'neotree-enter-horizontal-split
   :n "q"         #'neotree-hide
   :n "R"         #'neotree-refresh)

 ;; realgud
 (:after realgud
   :map realgud:shortkey-mode-map
   :n "j" #'evil-next-line
   :n "k" #'evil-previous-line
   :n "h" #'evil-backward-char
   :n "l" #'evil-forward-char
   :m "n" #'realgud:cmd-next
   :m "b" #'realgud:cmd-break
   :m "B" #'realgud:cmd-clear
   :n "c" #'realgud:cmd-continue)

 ;; rotate-text
 :n  "gs"  #'rotate-text

 ;; smart-forward
 :m  "g]" #'smart-forward
 :m  "g[" #'smart-backward

 ;; undo-tree -- undo/redo for visual regions
 :v "C-u" #'undo-tree-undo
 :v "C-r" #'undo-tree-redo

 ;; yasnippet
 (:after yasnippet
   (:map yas-keymap
     "C-e"           #'+snippets/goto-end-of-field
     "C-a"           #'+snippets/goto-start-of-field
     "<M-right>"     #'+snippets/goto-end-of-field
     "<M-left>"      #'+snippets/goto-start-of-field
     "<M-backspace>" #'+snippets/delete-to-start-of-field
     [escape]        #'evil-normal-state
     [backspace]     #'+snippets/delete-backward-char
     [delete]        #'+snippets/delete-forward-char-or-field)
   (:map yas-minor-mode-map
     :i "<tab>" yas-maybe-expand
     :v "<tab>" #'+snippets/expand-on-region))


 ;; --- Major mode bindings --------------------------

 ;; Markdown
 (:after markdown-mode
   (:map markdown-mode-map
     ;; fix conflicts with private bindings
     "<backspace>" nil
     "<M-left>"    nil
     "<M-right>"   nil))

 ;; Rust
 (:after rust
   (:map rust-mode-map
     "K"     #'racer-describe
     "g RET" #'cargo-process-test))

 ;; Elixir
 (:after alchemist
   (:map elixir-mode-map
     :n "K"     #'alchemist-help-search-at-point
     :n "g RET" #'alchemist-project-run-tests-for-current-file
     :n "g \\"  #'alchemist-mix-test-at-point
     :n "g SPC" #'alchemist-mix-compile))

 ;; Haskell
 (:after haskell-mode
   (:map haskell-mode-map
     ;; :n "K"     #'intero-info
     :n "K"     #'lsp-describe-thing-at-point
     ;; :n "g d"   #'lsp-ui-peek-find-definitions
     :n "g d"   #'lsp-ui-peek-find-definitions
     ;; :n "g SPC" #'intero-repl-load
     ;; :n "g y"   #'lsp-ui-
     ))

 ;; Javascript
 ;; (:after rjsx-mode
 ;;   (:map rjsx-mode-map
 ;;     :n "g d" #'flow-minor-jump-to-definition
 ;;     :n "K"   #'flow-minor-type-at-pos))

 (:after js2-mode
   (:map js2-mode-map
     :n "g d" #'flow-minor-jump-to-definition
     :n "K"   #'flow-minor-type-at-pos))

 ;; Elisp
 (:map emacs-lisp-mode-map
   :n "g SPC" #'eval-buffer
   :n "g RET" (λ! () (ert t)))


 ;; --- Custom evil text-objects ---------------------
 :textobj "a" #'evil-inner-arg                    #'evil-outer-arg
 :textobj "B" #'evil-textobj-anyblock-inner-block #'evil-textobj-anyblock-a-block
 :textobj "i" #'evil-indent-plus-i-indent         #'evil-indent-plus-a-indent
 :textobj "I" #'evil-indent-plus-i-indent-up      #'evil-indent-plus-a-indent-up
 :textobj "J" #'evil-indent-plus-i-indent-up-down #'evil-indent-plus-a-indent-up-down


 ;; --- Built-in plugins -----------------------------
 (:after comint
   ;; TAB auto-completion in term buffers
   :map comint-mode-map [tab] #'company-complete)

 (:after debug
   ;; For elisp debugging
   :map debugger-mode-map
   :n "RET" #'debug-help-follow
   :n "e"   #'debugger-eval-expression
   :n "n"   #'debugger-step-through
   :n "c"   #'debugger-continue)

 (:map help-mode-map
   :n "[["  #'help-go-back
   :n "]]"  #'help-go-forward
   :n "o"   #'ace-link-help
   :n "q"   #'quit-window
   :n "Q"   #'+ivy-quit-and-resume)

 (:after vc-annotate
   :map vc-annotate-mode-map
   :n "q"   #'kill-this-buffer
   :n "d"   #'vc-annotate-show-diff-revision-at-line
   :n "D"   #'vc-annotate-show-changeset-diff-revision-at-line
   :n "SPC" #'vc-annotate-show-log-revision-at-line
   :n "]]"  #'vc-annotate-next-revision
   :n "[["  #'vc-annotate-prev-revision
   :n "TAB" #'vc-annotate-toggle-annotation-visibility
   :n "RET" #'vc-annotate-find-revision-at-line))

;; evil-easymotion
(after! evil-easymotion
  (let ((prefix (concat doom-leader-key " /")))
    ;; NOTE `evilem-default-keybinds' unsets all other keys on the prefix (in
    ;; motion state)
    (evilem-default-keybindings prefix)
    (evilem-define (kbd (concat prefix " n")) #'evil-ex-search-next)
    (evilem-define (kbd (concat prefix " N")) #'evil-ex-search-previous)
    (evilem-define (kbd (concat prefix " s")) #'evil-snipe-repeat
                   :pre-hook (save-excursion (call-interactively #'evil-snipe-s))
                   :bind ((evil-snipe-scope 'buffer)
                          (evil-snipe-enable-highlight)
                          (evil-snipe-enable-incremental-highlight)))
    (evilem-define (kbd (concat prefix " S")) #'evil-snipe-repeat-reverse
                   :pre-hook (save-excursion (call-interactively #'evil-snipe-s))
                   :bind ((evil-snipe-scope 'buffer)
                          (evil-snipe-enable-highlight)
                          (evil-snipe-enable-incremental-highlight)))))


;;
;; Keybinding fixes
;;

;; This section is dedicated to "fixing" certain keys so that they behave
;; properly, more like vim, or how I like it.

(map! (:map input-decode-map
        [S-iso-lefttab] [backtab]
        (:unless window-system "TAB" [tab])) ; Fix TAB in terminal

      ;; I want C-a and C-e to be a little smarter. C-a will jump to
      ;; indentation. Pressing it again will send you to the true bol. Same goes
      ;; for C-e, except it will ignore comments and trailing whitespace before
      ;; jumping to eol.
      :i "C-a" #'doom/backward-to-bol-or-indent
      :i "C-e" #'doom/forward-to-last-non-comment-or-eol
      :i "C-u" #'doom/backward-kill-to-bol-and-indent

      ;; Emacsien motions for insert mode
      :i "C-b" #'backward-word
      :i "C-f" #'forward-word

      ;; Highjacks space/backspace to:
      ;;   a) balance spaces inside brackets/parentheses ( | ) -> (|)
      ;;   b) delete space-indented blocks intelligently
      ;;   c) do none of this when inside a string
      ;; :i "SPC"                          #'doom/inflate-space-maybe
      ;; :i [remap delete-backward-char]   #'doom/deflate-space-maybe
      ;; :i [remap newline]                #'doom/newline-and-indent

      (:after org
        (:map org-mode-map
          :i [remap doom/inflate-space-maybe] #'org-self-insert-command
          ))

      ;; Restore common editing keys (and ESC) in minibuffer
      (:map (minibuffer-local-map
             minibuffer-local-ns-map
             minibuffer-local-completion-map
             minibuffer-local-must-match-map
             minibuffer-local-isearch-map
             evil-ex-completion-map
             evil-ex-search-keymap
             read-expression-map)
        ;; [escape] #'abort-recursive-edit
        "C-r" #'evil-paste-from-register
        "C-a" #'move-beginning-of-line
        "C-w" #'doom/minibuffer-kill-word
        "C-u" #'doom/minibuffer-kill-line
        "C-b" #'backward-word
        "C-f" #'forward-word
        "M-z" #'doom/minibuffer-undo)

      (:map messages-buffer-mode-map
        "M-;" #'eval-expression
        "A-;" #'eval-expression)

      (:map tabulated-list-mode-map
        [remap evil-record-macro] #'doom/popup-close-maybe)

      (:after view
        (:map view-mode-map "<escape>" #'View-quit-all)))

(defun +sexp-transpose ()
  (interactive)
  (case evil-this-operator
    ('evil-shift-right (paxedit-transpose-forward))
    ('evil-shift-left  (paxedit-transpose-backward))))

;; (defun nmap (&rest keys-and-ops)
;;   (->>
;;    (seq-partition keys-and-ops 2)
;;    (seq-map
;;     (lambda (k-op)
;;       (let* ((k (car k-op))
;;              (op (cadr k-op))
;;              (prefix (substring k 0 1))
;;              (prefix-sym (lookup-key evil-normal-state-map prefix))
;;              (keyseq (substring k 1)))
;;         (list keyseq prefix-sym op))))
;;    (seq-group-by #'car)
;;    (seq-map
;;     (lambda (k-ops)
;;       (let* ((keyseq           (car k-ops))
;;              (ops              (cdr k-ops))
;;              (existing-binding (lookup-key evil-operator-state-map keyseq))
;;              (handler (λ! ()
;;                           (if-let
;;                               ((oplist
;;                                 (seq-find (lambda (op)
;;                                             (equal (nth 1 op)
;;                                                    evil-this-operator))
;;                                           ops)))
;;                               (message "calling oplist")
;;                               (->> oplist (nth 2) funcall)
;;                             (when existing-binding
;;                               (funcall existing-binding))))))
;;         (if existing-binding
;;             (progn
;;               (define-key evil-operator-state-map
;;                 (vector 'remap existing-binding)
;;                 handler)
;;               (define-key evil-motion-state-map
;;                 (vector 'remap existing-binding)
;;                 handler))
;;           (define-key evil-operator-state-map keyseq handler)))))))

;; (nmap
;;  ">e" #'paxedit-transpose-forward
;;  "<e" #'paxedit-transpose-backward)

(require 'paxedit)
(require 'general)
(general-evil-setup t)

(nmap
  ">" (general-key-dispatch 'evil-shift-right
        "e" 'paxedit-transpose-forward
        ")" 'sp-forward-slurp-sexp
        "(" 'sp-backward-barf-sexp
        "I" 'grfn/insert-at-sexp-end
        ;; "a" 'grfn/insert-at-form-end
        ))

(nmap
  "<" (general-key-dispatch 'evil-shift-left
        "e" 'paxedit-transpose-backward
        ")" 'sp-forward-barf-sexp
        "(" 'sp-backward-slurp-sexp
        "I" 'grfn/insert-at-sexp-start
        ;; "a" 'grfn/insert-at-form-start
        ))


(defmacro saving-excursion (&rest body)
  `(λ! () (save-excursion ,@body)))

(nmap "c" (general-key-dispatch 'evil-change
            "r c" (saving-excursion (string-inflection-lower-camelcase))
            "r C" (saving-excursion (string-inflection-camelcase))
            "r m" (saving-excursion (string-inflection-camelcase))
            "r s" (saving-excursion (string-inflection-underscore))
            "r u" (saving-excursion (string-inflection-upcase))
            "r -" (saving-excursion (string-inflection-kebab-case))
            "r k" (saving-excursion (string-inflection-kebab-case))
            ;; "r ." (saving-excursion (string-inflection-dot-case))
            ;; "r ." (saving-excursion (string-inflection-space-case))
            ;; "r ." (saving-excursion (string-inflection-title-case))
            ))


(predd-defmulti eval-sexp (lambda (form) major-mode))

(predd-defmethod eval-sexp 'clojure-mode (form)
  (cider-interactive-eval form))

(predd-defmethod eval-sexp 'emacs-lisp-mode (form)
  (pp-eval-expression form))

(predd-defmulti eval-sexp-region (lambda (_beg _end) major-mode))

(predd-defmethod eval-sexp-region 'clojure-mode (beg end)
  (cider-interactive-eval nil nil (list beg end)))

(predd-defmethod eval-sexp-region 'emacs-lisp-mode (beg end)
  (pp-eval-expression (read (buffer-substring beg end))))

(predd-defmulti eval-sexp-region-context (lambda (_beg _end _context) major-mode))

(predd-defmethod eval-sexp-region-context 'clojure-mode (beg end context)
  (cider--eval-in-context (buffer-substring beg end)))

(defun pp-eval-context-region (beg end context)
  (interactive "r\nxContext: ")
  (let* ((inner-expr (read (buffer-substring beg end)))
         (full-expr (list 'let* context inner-expr)))
    (pp-eval-expression full-expr)))

(predd-defmethod eval-sexp-region-context 'emacs-lisp-mode (beg end context)
  (pp-eval-context-region beg end context))

(predd-defmulti preceding-sexp (lambda () major-mode))

(predd-defmethod preceding-sexp 'clojure-mode ()
  (cider-last-sexp))

(predd-defmethod preceding-sexp 'emacs-lisp-mode ()
  (elisp--preceding-sexp))

(defun eval-sexp-at-point ()
  (interactive)
  (let ((bounds (bounds-of-thing-at-point 'sexp)))
    (eval-sexp-region (car bounds)
                      (cdr bounds))))

(defun eval-last-sexp ()
  (interactive)
  (eval-sexp (preceding-sexp)))

;;;

(defun cider-insert-current-sexp-in-repl (&optional arg)
  "Insert the expression at point in the REPL buffer.
If invoked with a prefix ARG eval the expression after inserting it"
  (interactive "P")
  (cider-insert-in-repl (cider-sexp-at-point) arg))

(evil-define-operator fireplace-send (beg end)
  (cider-insert-current-sexp-in-repl nil nil (list beg end)))

(defun +clojure-pprint-expr (form)
  (format "(with-out-str (clojure.pprint/pprint %s))"
          form))

(defun cider-eval-read-and-print-handler (&optional buffer)
  "Make a handler for evaluating and reading then printing result in BUFFER."
  (nrepl-make-response-handler
   (or buffer (current-buffer))
   (lambda (buffer value)
     (let ((value* (read value)))
       (with-current-buffer buffer
         (insert
          (if (derived-mode-p 'cider-clojure-interaction-mode)
              (format "\n%s\n" value*)
            value*)))))
   (lambda (_buffer out) (cider-emit-interactive-eval-output out))
   (lambda (_buffer err) (cider-emit-interactive-eval-err-output err))
   '()))

(defun cider-eval-and-replace (beg end)
  "Evaluate the expression in region and replace it with its result"
  (interactive "r")
  (let ((form (buffer-substring beg end)))
    (cider-nrepl-sync-request:eval form)
    (kill-region beg end)
    (cider-interactive-eval
     (+clojure-pprint-expr form)
     (cider-eval-read-and-print-handler))))

(defun cider-eval-current-sexp-and-replace ()
  "Evaluate the expression at point and replace it with its result"
  (interactive)
  (apply #'cider-eval-and-replace (cider-sexp-at-point 'bounds)))

;;;

(evil-define-operator fireplace-eval (beg end)
  (eval-sexp-region beg end))

(evil-define-operator fireplace-replace (beg end)
  (cider-eval-and-replace beg end))

(evil-define-operator fireplace-eval-context (beg end)
  (eval-sexp-region-context beg end))

;;; fireplace-esque eval binding
(nmap :keymaps 'cider-mode-map
  "c" (general-key-dispatch 'evil-change
        "p" (general-key-dispatch 'fireplace-eval
              "p" 'cider-eval-sexp-at-point
              "c" 'cider-eval-last-sexp
              "d" 'cider-eval-defun-at-point
              "r" 'cider-test-run-test)
        "q" (general-key-dispatch 'fireplace-send
              "q" 'cider-insert-current-sexp-in-repl
              "c" 'cider-insert-last-sexp-in-repl)
        "x" (general-key-dispatch 'fireplace-eval-context
              "x" 'cider-eval-sexp-at-point-in-context
              "c" 'cider-eval-last-sexp-in-context)
        "!" (general-key-dispatch 'fireplace-replace
              "!" 'cider-eval-current-sexp-and-replace
              "c" 'cider-eval-last-sexp-and-replace)
        "y" 'cider-copy-last-result))

;;;

(nmap :keymaps 'emacs-lisp-mode-map
  "c" (general-key-dispatch 'evil-change
        "p" (general-key-dispatch 'fireplace-eval
              "p" 'eval-sexp-at-point
              "c" 'eval-last-sexp
              "d" 'eval-defun
              "r" 'cider-test-run-test)
        "x" (general-key-dispatch 'fireplace-eval-context
              "x" 'cider-eval-sexp-at-point-in-context
              "c" 'cider-eval-last-sexp-in-context)
        "!" (general-key-dispatch 'fireplace-replace
              "!" 'cider-eval-current-sexp-and-replace
              "c" 'cider-eval-last-sexp-and-replace)
        "y" 'cider-copy-last-result))


;; >) ; slurp forward
;; <) ; barf forward
;; <( ; slurp backward
;; >( ; slurp backward

;; (require 'doom-themes)
(defun grfn/haskell-test-file-p ()
  (string-match-p (rx (and "Spec.hs" eol))
                  (buffer-file-name)))

(require 'haskell)

(defun grfn/intero-run-main ()
  (interactive)
  (intero-repl-load)
  (intero-with-repl-buffer nil
    (comint-simple-send
     (get-buffer-process (current-buffer))
     "main")))

(defun grfn/run-clj-or-cljs-test ()
  (interactive)
  (message "Running tests...")
  (cl-case (cider-repl-type-for-buffer)
    ('cljs
     (cider-interactive-eval
      "(with-out-str (cljs.test/run-tests))"
      (nrepl-make-response-handler
       (current-buffer)
       (lambda (_ value)
         (with-output-to-temp-buffer "*cljs-test-results*"
           (print
            (->> value
                 (s-replace "\"" "")
                 (s-replace "\\n" "\n")))))
       nil nil nil)))
    ('clj
     (funcall-interactively
      #'cider-test-run-ns-tests
      nil))))

(defun cider-copy-last-result ()
  (interactive)
  (cider-interactive-eval
   "*1"
   (nrepl-make-response-handler
    (current-buffer)
    (lambda (_ value)
      (kill-new value)
      (message "Copied last result (%s) to clipboard"
               (if (= (length value) 1) "1 char"
                 (format "%d chars" (length value)))))
    nil nil nil)))


(defun grfn/insert-new-src-block ()
  (interactive)
  (let* ((current-src-block (org-element-at-point))
         (src-block-head (save-excursion
                           (goto-char (org-element-property
                                       :begin current-src-block))
                           (let ((line (thing-at-point 'line t)))
                             (if (not (s-starts-with? "#+NAME:" (s-trim line)))
                                 line
                               (forward-line)
                               (thing-at-point 'line t)))))
         (point-to-insert
          (if-let (results-loc (org-babel-where-is-src-block-result))
              (save-excursion
                (goto-char results-loc)
                (org-element-property
                 :end
                 (org-element-at-point)))
            (org-element-property :end (org-element-at-point)))))
    (goto-char point-to-insert)
    (insert "\n")
    (insert src-block-head)
    (let ((contents (point-marker)))
      (insert "\n#+END_SRC\n")
      (goto-char contents))))

(defun grfn/+org-insert-item (orig direction)
  (interactive)
  (if (and (org-in-src-block-p)
           (equal direction 'below))
    (grfn/insert-new-src-block)
    (funcall orig direction)))

(advice-add #'+org--insert-item :around #'grfn/+org-insert-item)
;; (advice-add #'+org/insert-item-below :around
;;             (lambda (orig) (grfn/+org-insert-item orig 'below)))

(defun set-pdb-trace ()
  (interactive)
  (end-of-line)
  (insert (format "\n%simport pdb;pdb.set_trace()"
                  (make-string (python-indent-calculate-indentation)
                               ?\s)))
  (evil-indent (line-beginning-position)
               (line-end-position)))

(map!

 (:map magit-mode-map
   :n "#" 'forge-dispatch)

 (:map haskell-mode-map
   :n "K"     'lsp-info-under-point
   :n "g d"   'lsp-ui-peek-find-definitions
   :n "g r"   'lsp-ui-peek-find-references
   :n "g \\"  '+haskell/repl
   ;; :n "K"     'intero-info
   ;; :n "g d"   'intero-goto-definition
   ;; :n "g SPC" 'intero-repl-load
   ;; :n "g \\"  'intero-repl
   ;; :n "g y"   'intero-type-at
   ;; :n "g RET" 'grfn/run-sputnik-test-for-file

   (:localleader
     :desc "Apply action"  :n "e" 'intero-repl-eval-region
     :desc "Rename symbol" :n "r" 'intero-apply-suggestions))

 (:map python-mode-map
   :n "K" #'anaconda-mode-show-doc
   :n "g SPC" #'+eval/buffer
   :n "g RET" #'python-pytest-file
   :n "g \\" #'+python/open-ipython-repl
   [remap evil-commentary-yank] #'set-pdb-trace)

 (:after agda2-mode
   (:map agda2-mode-map
     :n "g SPC" 'agda2-load
     :n "g d"   'agda2-goto-definition-keyboard
     :n "] g"   'agda2-next-goal
     :n "[ g"   'agda2-previous-goal

     (:localleader
       :desc "Give"                               :n "SPC" 'agda2-give
       :desc "Case Split"                         :n "c"   'agda2-make-case
       :desc "Make Helper"                        :n "h"   'agda2-helper-function-type
       :desc "Refine"                             :n "r"   'agda2-refine
       :desc "Auto"                               :n "a"   'agda2-auto-maybe-all
       :desc "Goal type and context"              :n "t"   'agda2-goal-and-context
       :desc "Goal type and context and inferred" :n ";"   'agda2-goal-and-context-and-inferred)))

 (:after clojure-mode
   (:map clojure-mode-map
     :n "] f" 'forward-sexp
     :n "[ f" 'backward-sexp))

 (:after cider-mode
   (:map cider-mode-map
     :n "g SPC" 'cider-eval-buffer
     :n "g \\"  'cider-switch-to-repl-buffer
     :n "K"     'cider-doc
     :n "g K"   'cider-grimoire
     :n "g d"   'cider-find-dwim
     :n "C-w ]" 'cider-find-dwim-other-window
     ;; :n "g RET" 'cider-test-run-ns-tests
     :n "g RET" 'grfn/run-clj-or-cljs-test

     "C-c C-r r" 'cljr-add-require-to-ns
     "C-c C-r i" 'cljr-add-import-to-ns

     (:localleader
       ;; :desc "Inspect last result" :n "i" 'cider-inspect-last-result
       ;; :desc "Search for documentation" :n "h s" 'cider-apropos-doc
       :desc "Add require to ns" :n "n r" 'cljr-add-require-to-ns
       :desc "Add import to ns" :n "n i" 'cljr-add-import-to-ns))
   (:map cider-repl-mode-map
     :n "g \\" 'cider-switch-to-last-clojure-buffer))

 (:after w3m
   (:map w3m-mode-map
     "/" 'evil-search-forward
     "?" 'evil-search-backward))

 (:after slack
   (:map slack-message-buffer-mode-map
     :i "<up>" #'slack-message-edit))

 (:after org
   :n "C-c C-x C-o" #'org-clock-out
   (:map org-mode-map
     [remap counsel-imenu] #'counsel-org-goto
     "M-k" #'org-move-subtree-up
     "M-j" #'org-move-subtree-down
     (:localleader
       :n "g" #'counsel-org-goto))

   (:map org-capture-mode-map
     :n "g RET" #'org-capture-finalize
     :n "g \\"  #'org-captue-refile))

 (:map lsp-mode-map
   :n "K"   #'lsp-describe-thing-at-point
   :n "g r" #'lsp-rename
   (:localleader
     :n "a" #'lsp-execute-code-action)))
