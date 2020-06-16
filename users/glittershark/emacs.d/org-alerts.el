;;; ~/.doom.d/org-alerts.el -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 's)
(require 'dash)
(require 'alert)
(require 'org-agenda)


(defvar grfn/org-alert-interval 300
  "Interval in seconds to recheck and display deadlines.")


(defvar grfn/org-alert-notification-title "*org*"
  "Title to be sent with notify-send.")

(defvar grfn/org-alert-headline-regexp "\\(Sched.+:.+\\|Deadline:.+\\)"
  "Regexp for headlines to search in agenda buffer.")

(defun grfn/org-alert--strip-prefix (headline)
  "Remove the scheduled/deadline prefix from HEADLINE."
  (replace-regexp-in-string ".*:\s+" "" headline))


(defun grfn/org-alert--unique-headlines (regexp agenda)
  "Return unique headlines from the results of REGEXP in AGENDA."
  (let ((matches (-distinct (-flatten (s-match-strings-all regexp agenda)))))
    (--map (grfn/org-alert--strip-prefix it) matches)))


(defun grfn/org-alert--get-headlines ()
  "Return the current org agenda as text only."
  (with-temp-buffer
    (let ((org-agenda-sticky nil)
          (org-agenda-buffer-tmp-name (buffer-name)))
      (ignore-errors (org-agenda-list nil "TODAY" 1))
      (grfn/org-alert--unique-headlines
       grfn/org-alert-headline-regexp
       (buffer-substring-no-properties (point-min) (point-max))))))

(defun grfn/parse-range-string (str)
  (when
      (string-match (rx (group (repeat 2 (any digit))
                               ":"
                               (repeat 2 (any digit)))
                        (optional
                         (and
                          "-"
                          (group (repeat 2 (any digit))
                                 ":"
                                 (repeat 2 (any digit))))))
                    str)
    (list
     (org-read-date nil t
                    (match-string 1 str))
     (when-let ((et (match-string 2 str))) (org-read-date nil t et)))))

(defun grfn/start-time-from-range-string (str)
  (pcase-let ((`(,start-time . _) (grfn/parse-range-string str)))
    start-time))

(comment
 (org-agenda-list nil "TODAY" 1)

 (grfn/org-alert--get-headlines)
 (setq --src
       (with-temp-buffer
         (let ((org-agenda-sticky nil)
               (org-agenda-buffer-tmp-name (buffer-name)))
           (ignore-errors (org-agenda-list nil "TODAY" 1))
           (buffer-substring-no-properties (point-min) (point-max)))))

 (setq --entries
       (with-temp-buffer
         (let ((inhibit-redisplay t)
               (org-agenda-sticky nil)
               (org-agenda-buffer-tmp-name (buffer-name))
               (org-agenda-buffer-name (buffer-name))
               (org-agenda-buffer (current-buffer)))
           (org-agenda-get-day-entries
            (cadr (org-agenda-files nil 'ifmode))
            (calendar-gregorian-from-absolute
             (time-to-days (org-read-date nil t "TODAY")))))))

 (loop for k in (text-properties-at 0 (car --entries))
       by #'cddr
       collect k)

 (--map (substring-no-properties (get-text-property 0 'txt it)) --entries)
 (--map (get-text-property 0 'time it) --entries)
 (current-time)

 (format-time-string "%R" (org-read-date nil t "10:00-11:00"))

 (grfn/start-time-from-range-string "10:00")

 (current-time-string (org-read-date nil t "10:00-11:00"))

 (todo-state
  org-habit-p
  priority
  warntime
  ts-date
  date
  type
  org-hd-marker
  org-marker
  face
  undone-face
  help-echo
  mouse-face
  done-face
  org-complex-heading-regexp
  org-todo-regexp
  org-not-done-regexp
  dotime
  format
  extra
  time
  level
  txt
  breadcrumbs
  duration
  time-of-day
  org-lowest-priority
  org-highest-priority
  tags
  org-category)

 (propertize)

 --src
 )


(defun grfn/org-alert--headline-complete? (headline)
  "Return whether HEADLINE has been completed."
  (--any? (s-starts-with? it headline) org-done-keywords-for-agenda))


(defun grfn/org-alert--filter-active (deadlines)
  "Remove any completed headings from the provided DEADLINES."
  (-remove 'grfn/org-alert--headline-complete? deadlines))


(defun grfn/org-alert--strip-states (deadlines)
  "Remove the todo states from DEADLINES."
  (--map (s-trim (s-chop-prefixes org-todo-keywords-for-agenda it)) deadlines))


(defun grfn/org-alert-check ()
  "Check for active, due deadlines and initiate notifications."
  (interactive)
  ;; avoid interrupting current command.
  (unless (minibufferp)
    (save-window-excursion
      (save-excursion
        (save-restriction
          (let ((active (grfn/org-alert--filter-active (grfn/org-alert--get-headlines))))
            (dolist (dl (grfn/org-alert--strip-states active))
              (alert dl :title grfn/org-alert-notification-title))))))
    (when (get-buffer org-agenda-buffer-name)
      (ignore-errors
        (with-current-buffer org-agenda-buffer-name
          (org-agenda-redo t))))))


(defun grfn/org-alert-enable ()
  "Enable the notification timer.  Cancels existing timer if running."
  (interactive)
  (grfn/org-alert-disable)
  (run-at-time 0 grfn/org-alert-interval 'grfn/org-alert-check))


(defun grfn/org-alert-disable ()
  "Cancel the running notification timer."
  (interactive)
  (dolist (timer timer-list)
    (if (eq (elt timer 5) 'grfn/org-alert-check)
        (cancel-timer timer))))



(provide 'grfn/org-alert)
;;; grfn/org-alert.el ends here
