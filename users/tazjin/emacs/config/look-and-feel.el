;;; -*- lexical-binding: t; -*-

;; Hide those ugly tool bars:
(tool-bar-mode 0)
(scroll-bar-mode 0)
(menu-bar-mode 0)
(add-hook 'after-make-frame-functions
          (lambda (frame) (scroll-bar-mode 0)))

;; Don't do any annoying things:
(setq ring-bell-function 'ignore)
(setq initial-scratch-message "")

;; Usually emacs will run as a proper GUI application, in which case a few
;; extra settings are nice-to-have:
(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (mouse-wheel-mode t)
  (blink-cursor-mode -1))

;; Configure Emacs fonts.
(let ((font (format "JetBrains Mono-%d" 12)))
  (setq default-frame-alist `((font . ,font)))
  (set-frame-font font t t))

;; Configure the modeline

;; Implements a mode-line warning if there are any logged in TTY
;; sessions apart from the graphical one.
;;
;; The status is only updated once every 30 seconds, as it requires
;; shelling out to some commands (for now).
(defun list-tty-sessions ()
  "List all logged in tty sessions, except tty7 (graphical)"
  (let ((command "who | awk '{print $2}' | grep -v tty7"))
    (-filter (lambda (s) (not (string-empty-p s)))
             (s-lines
              (s-trim (let ((default-directory "/"))
                        (shell-command-to-string command)))))))

(defvar cached-tty-sessions (cons (time-convert nil 'integer) (list-tty-sessions))
   "Cached TTY session value to avoid running the command too often.")

;; TODO(tazjin): add this to the modeline

(defun get-cached-tty-sessions ()
  (let ((time ))
    (when (< 30
             (- (time-convert nil 'integer)
                (car cached-tty-sessions)))
      (setq cached-tty-sessions
            (cons (time-convert nil 'integer) (list-tty-sessions)))))

  (cdr cached-tty-sessions))

;; Auto refresh buffers
(global-auto-revert-mode 1)

;; Use clipboard properly
(setq select-enable-clipboard t)

;; Show in-progress chords in minibuffer
(setq echo-keystrokes 0.1)

;; Show column numbers in all buffers
(column-number-mode t)

(defalias 'yes-or-no-p 'y-or-n-p)
(defalias 'auto-tail-revert-mode 'tail-mode)

;; Style line numbers (shown with M-g g)
(setq linum-format
      (lambda (line)
        (propertize
         (format (concat " %"
                         (number-to-string
                          (length (number-to-string
                                   (line-number-at-pos (point-max)))))
                         "d ")
                 line)
         'face 'linum)))

;; Display tabs as 2 spaces
(setq tab-width 2)

;; Don't wrap around when moving between buffers
(setq windmove-wrap-around nil)

;; Don't show me all emacs warnings immediately. Unfortunately this is
;; not very granular, as emacs displays most of its warnings in the
;; `emacs' "category", but without it every time I
;; fullscreen/unfullscreen the warning buffer destroys my layout.
;;
;; Warnings suppressed by this are still logged to the warnings
;; buffer.
(setq warning-suppress-types '((emacs)))

(provide 'look-and-feel)
