;;; ~/.doom.d/org-query.el -*- lexical-binding: t; -*-

(require 'org)
(require 'org-agenda)
(require 'inflections)

(defun grfn/org-agenda-entry->element (agenda-entry)
  ;; ???
  ())

(defun org-elements-agenda-match (match &optional todo-only)
  (setq match
        (propertize match 'inherited t))
  (with-temp-buffer
    (let ((inhibit-redisplay (not debug-on-error))
          (org-agenda-sticky nil)
          (org-agenda-buffer-tmp-name (buffer-name))
          (org-agenda-buffer-name (buffer-name))
          (org-agenda-buffer (current-buffer))
          (matcher (org-make-tags-matcher match))
          result)
      (org-agenda-prepare (concat "TAGS " match))
      (setq match (car matcher)
            matcher (cdr matcher))
      (dolist (file (org-agenda-files nil 'ifmode)
                    result)
        (catch 'nextfile
          (org-check-agenda-file file)
          (when-let ((buffer (if (file-exists-p file)
                                 (org-get-agenda-file-buffer file)
                               (error "No such file %s" file))))
            (with-current-buffer buffer
              (unless (derived-mode-p 'org-mode)
                (error "Agenda file %s is not in Org mode" file))
              (save-excursion
                (save-restriction
                  (if (eq buffer org-agenda-restrict)
                      (narrow-to-region org-agenda-restrict-begin
                                        org-agenda-restrict-end)
                    (widen))
                  (setq result
                        (append result (org-scan-tags
                                        'agenda
                                        matcher
                                        todo-only))))))))))))

(defun grfn/num-inbox-items ()
  (length (org-elements-agenda-match "inbox" t)))

(defun grfn/num-inbox-items-message ()
  (let ((n (grfn/num-inbox-items)))
    (unless (zerop n)
      (format "%d %s"
              n
              (if (= 1 n) "item" "items")))))

(defmacro grfn/at-org-clocked-in-item (&rest body)
  `(when (org-clocking-p)
     (let ((m org-clock-marker))
       (with-current-buffer (marker-buffer m)
         (save-mark-and-excursion
           (goto-char m)
           (org-back-to-heading t)
           ,@body)))))

(defun grfn/org-element-clocked-in-task ()
  (grfn/at-org-clocked-in-item
   (org-element-at-point)))

(comment
 (grfn/org-element-clocked-in-task)
 (org-element-property :title (grfn/org-element-clocked-in-task))
 )

(defun grfn/minutes->hours:minutes (minutes)
  (format "%d:%02d"
          (floor (/ minutes 60))
          (mod minutes 60)))

(comment
 (grfn/minutes->hours:minutes 1)        ; => "0:01"
 (grfn/minutes->hours:minutes 15)       ; => "0:15"
 (grfn/minutes->hours:minutes 130)      ; => "2:10"
 )

(defun grfn/org-current-clocked-in-task-message ()
  (if (org-clocking-p)
      (format "(%s) [%s]"
              (org-element-property :title (grfn/org-element-clocked-in-task))
              (grfn/minutes->hours:minutes
               (org-clock-get-clocked-time)))
    ""))

(comment
 (grfn/org-current-clocked-in-task-message)
 )
