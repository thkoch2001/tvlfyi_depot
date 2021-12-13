;;; modeline.el --- Customize my mode-line -*- lexical-binding: t -*-

;; Author: William Carroll <wpcarro@gmail.com>
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1"))
;; Homepage: https://user.git.corp.google.com/wpcarro/briefcase

;;; Commentary:
;; Because I use EXWM, I treat my Emacs mode-line like my system bar: I need to
;; quickly check the system time, and I expect it to be at the bottom-right of
;; my Emacs frame.  I used doom-modeline for awhile, which is an impressive
;; package, but it conditionally colorizes on the modeline for the active
;; buffer.  So if my bottom-right window is inactive, I cannot see the time.
;;
;; My friend, @tazjin, has a modeline setup that I think is more compatible with
;; EXWM, so I'm going to base my setup off of his.

;;; Code:

(use-package telephone-line)

(defun modeline-bottom-right-window? ()
  "Determines whether the last (i.e.
bottom-right) window of the
active frame is showing the buffer in which this function is
  executed."
  (let* ((frame (selected-frame))
         (right-windows (window-at-side-list frame 'right))
         (bottom-windows (window-at-side-list frame 'bottom))
         (last-window (car (seq-intersection right-windows bottom-windows))))
    (eq (current-buffer) (window-buffer last-window))))

(defun modeline-maybe-render-time ()
  "Conditionally renders the `mode-line-misc-info' string.

  The idea is to not display information like the current time,
  load, battery levels on all buffers."
  (when (modeline-bottom-right-window?)
    (telephone-line-raw mode-line-misc-info t)))

(defun modeline-setup ()
  "Render my custom modeline."
  (telephone-line-defsegment telephone-line-last-window-segment ()
    (modeline-maybe-render-time))
  ;; Display the current EXWM workspace index in the mode-line
  (telephone-line-defsegment telephone-line-exwm-workspace-index ()
    (when (modeline-bottom-right-window?)
      (format "[%s]" exwm-workspace-current-index)))
  ;; Define a highlight font for ~ important ~ information in the last
  ;; window.
  (defface special-highlight
    '((t (:foreground "white" :background "#5f627f"))) "")
  (add-to-list 'telephone-line-faces
               '(highlight . (special-highlight . special-highlight)))
  (setq telephone-line-lhs
        '((nil . (telephone-line-position-segment))
          (accent . (telephone-line-buffer-segment))))
  (setq telephone-line-rhs
        '((accent . (telephone-line-major-mode-segment))
          (nil . (telephone-line-last-window-segment
                  telephone-line-exwm-workspace-index))))
  (setq telephone-line-primary-left-separator 'telephone-line-tan-left
        telephone-line-primary-right-separator 'telephone-line-tan-right
        telephone-line-secondary-left-separator 'telephone-line-tan-hollow-left
        telephone-line-secondary-right-separator 'telephone-line-tan-hollow-right)
  (telephone-line-mode 1))

(provide 'modeline)
;;; modeline.el ends here
