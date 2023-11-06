;;; -*- lexical-binding: t; -*-

(require 'org)
(require 'org-agenda)
(require 'inflections)

(defun grfn/org-text-element->string (elt)
  (cond
   ((stringp elt) elt)
   ((and (consp elt)
         (symbolp (car elt)))
    (-> elt (caddr) (grfn/org-text-element->string) (s-trim) (concat " ")))))

(defun grfn/org-element-title (elt)
  (let ((title (org-element-property :title elt)))
    (cond
     ((stringp title) title)
     ((listp title)
      (->> title
           (mapcar #'grfn/org-text-element->string)
           (s-join "")
           (s-trim))))))

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
    (if (zerop n) ""
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
              (->> (grfn/org-element-clocked-in-task)
                   (grfn/org-element-title)
                   (substring-no-properties)
                   (s-trim))
              (grfn/minutes->hours:minutes
               (org-clock-get-clocked-time)))
    ""))

(comment
 (grfn/org-current-clocked-in-task-message)
 )

(cl-defgeneric grfn/org-tracker-ticket-id-label (backend elt)
  (org-tracker-backend/extract-issue-id backend elt))
(cl-defmethod grfn/org-tracker-ticket-id-label
  ((backend org-tracker-linear-backend) elt)
  (when-let* ((link (plist-get elt :LINEAR-KEY)))
    (string-match
     (rx "[[" (one-or-more anything) "]"
         "[" (group (one-or-more anything)) "]]")
     link)
    (match-string 1 link)))

(defun grfn/org-clocked-in-ticket-id ()
  (grfn/at-org-clocked-in-item
   (when-let* ((backend (org-tracker-current-backend t)))
     (grfn/org-tracker-ticket-id-label
      backend
      (cadr (org-element-at-point))))))

(comment
 (grfn/at-org-clocked-in-item
  (org-tracker-backend/extract-issue-id
   (org-tracker-current-backend)
   (cadr (org-element-at-point))))

 (grfn/org-clocked-in-ticket-id)
 )
