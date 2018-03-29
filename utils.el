;;; private/grfn/utils.el -*- lexical-binding: t; -*-


;; Elisp Extras

(defmacro comment (&rest _body)
  "Comment out one or more s-expressions"
  nil)

(defun inc (x) "Returns x + 1" (+ 1 x))
(defun dec (x) "Returns x - 1" (- x 1))


;;
;; Text editing utils
;;

;; Reading strings

(defun get-char (&optional point)
  "Get the character at the given `point' (defaulting to the current point),
without properties"
  (let ((point (or point (point))))
    (buffer-substring-no-properties point (+ 1 point))))

(defun get-line (&optional lineno)
  "Read the line number `lineno', or the current line if `lineno' is nil, and
return it as a string stripped of all text properties"
  (let ((current-line (line-number-at-pos)))
    (if (or (not lineno)
            (= current-line lineno))
        (thing-at-point 'line t)
      (save-mark-and-excursion
       (line-move (- lineno (line-number-at-pos)))
       (thing-at-point 'line t)))))

(defun get-line-point ()
  "Get the position in the current line of the point"
  (- (point) (line-beginning-position)))

;; Moving in the file

(defun goto-line-char (pt)
  "Moves the point to the given position expressed as an offset from the start
of the line"
  (goto-char (+ (line-beginning-position) pt)))

(defun goto-eol ()
  "Moves to the end of the current line"
  (goto-char (line-end-position)))

(defun goto-regex-on-line (regex)
  "Moves the point to the first occurrence of `regex' on the current line.
Returns nil if the regex did not match, non-nil otherwise"
  (when-let ((current-line (get-line))
             (line-char (string-match regex current-line)))
    (goto-line-char line-char)))

(defun goto-regex-on-line-r (regex)
  "Moves the point to the *last* occurrence of `regex' on the current line.
Returns nil if the regex did not match, non-nil otherwise"
  (when-let ((current-line (get-line))
             (modified-regex (concat ".*\\(" regex "\\)"))
             (_ (string-match modified-regex current-line))
             (match-start (match-beginning 1)))
    (goto-line-char match-start)))

(comment
 (progn
   (string-match (rx (and (zero-or-more anything)
                          (group "foo" "foo")))
                 "foofoofoo")
   (match-beginning 1)))

;; Changing file contents

(defun delete-line ()
  "Remove the line at the current point"
  (delete-region (line-beginning-position)
                 (inc (line-end-position))))

(defmacro modify-then-indent (&rest body)
  "Modify text in the buffer according to body, then re-indent from where the
  cursor started to where the cursor ended up, then return the cursor to where
  it started."
  `(let ((beg (line-beginning-position))
         (orig-line-char (- (point) (line-beginning-position))))
     (atomic-change-group
       (save-mark-and-excursion
        ,@body
        (evil-indent beg (+ (line-end-position) 1))))
     (goto-line-char orig-line-char)))
