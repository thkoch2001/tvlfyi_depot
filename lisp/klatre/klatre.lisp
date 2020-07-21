(in-package #:klatre)

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
  (declare (type (cons (simple-array character (*))) lst))
  (declare (type (simple-array character (*)) sep))
  (let ((vs (make-array 0
                        :element-type 'character
                        :fill-pointer 0
                        :adjustable t))
        (lsep (length sep)))
    (mapcar #'(lambda (str)
                (let ((nstr (funcall func str)))
                  (declare (type (simple-array character (*)) nstr))
                  (dotimes (j (length nstr) j)
                    (declare (type fixnum j))
                    (vector-push-extend (char nstr j) vs))
                  (dotimes (k lsep k)
                    (declare (type fixnum k))
                    (vector-push-extend (char sep k) vs))))
                lst)
    vs))

;;;
;;; String handling
;;;

(defun try-parse-integer (str)
  "Attempt to parse STR as an integer, returning nil if it is invalid."
  (declare (type (simple-array character (*)) str))
  (handler-case (parse-integer str)
    (sb-int:simple-parse-error (_) nil)))
