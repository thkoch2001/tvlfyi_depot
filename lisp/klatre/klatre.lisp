(in-package #:klatre)
(declaim (optimize (safety 3)))

(defmacro comment (&rest _))

(defun posp (n) (> n 0))

;;; Sequence utilities

(defun slice (vector start end)
  (make-array (- end start)
              :element-type (array-element-type vector)
              :displaced-to vector
              :displaced-index-offset start))

(defun chunk-vector (size vector &key start end sharedp)
  (check-type size (integer 1))
  (loop
     with slicer = (if sharedp #'slice #'subseq)
     and low = (or start 0)
     and high = (or end (length vector))
     for s from low below high by size
     for e from (+ low size) by size
     collect (funcall slicer vector s (min e high))))

(defun chunk-list/unbounded (size list)
  (loop
     for front = list then next
     for next = (nthcdr size front)
     collect (ldiff front next)
     while next))

(defun chunk-list/bounded (size list upper-limit)
  (loop
     for front = list then next
     for next = (nthcdr (min size upper-limit) front)
     collect (ldiff front next)
     do (decf upper-limit size)
     while (and next (plusp upper-limit))))

(defun chunk-list (size list &key (start 0) end)
  "Returns successive chunks of list of size SIZE, starting at START and ending
at END."
  (declare (inline check-list/bounded check-list/simple))
  (check-type size (integer 1))
  (let ((list (nthcdr start list)))
    (when list
      (if end
          (chunk-list/bounded size list (- end start))
          (chunk-list/unbounded size list)))))

(defun mapconcat (func lst sep)
  "Apply FUNC to each element of LST, and concat the results as strings,
separated by SEP."
  (check-type lst cons)
  (check-type sep (simple-array character (*)))
  (let ((vs (make-array 0
                        :element-type 'character
                        :fill-pointer 0
                        :adjustable t))
        (lsep (length sep)))
    (mapcar #'(lambda (str)
                (let ((nstr (the (simple-array character (*))
                                 (funcall func str))))
                  (dotimes (j (length nstr) j)
                    (vector-push-extend (char nstr (the fixnum j)) vs))
                  (dotimes (k lsep k)
                    (vector-push-extend (char sep (the fixnum k)) vs))))
                lst)
    vs))

;;;
;;; String handling
;;;

(defparameter dottime-format
  '((:year 4) #\- (:month 2) #\- (:day 2)
    #\T
    (:hour 2) #\· (:min 2) "+00") ; TODO(grfn): Allow passing offset
  "`:LOCAL-TIME' format specifier for dottime")

(defun format-dottime (timestamp)
  "Return TIMESTAMP formatted as dottime, using a +00 offset"
  (check-type timestamp local-time:timestamp)
  (local-time:format-timestring nil timestamp
                                :format dottime-format
                                :timezone local-time:+utc-zone+))

(comment
 (format-dottime (local-time:now)))

(defun try-parse-integer (str)
  "Attempt to parse STR as an integer, returning nil if it is invalid."
  (check-type str string)
  (handler-case (parse-integer str)
    (sb-int:simple-parse-error (_) nil)))
