(in-package :clhs-lookup)
(declaim (optimize (safety 3)))

(defun find-symbols-paths (syms clhs)
  "Find pathnames to HyperSpec files describing the listed
  symbol names (as strings). Paths are returned in the order
  of the symbols given with missing entries removed."
  (check-type syms list)
  (check-type clhs pathname)
  (let* ((data-dir (merge-pathnames "HyperSpec/Data/" clhs))
         (data (merge-pathnames "Map_Sym.txt" data-dir))
         (found (make-hash-table :test #'equal))
         (syms (mapcar #'string-upcase syms)))
  (with-open-file (s data :direction :input)
    (loop
      with missing    = syms
      for symbol-line = (read-line s nil :eof)
      for path-line   = (read-line s nil :eof)
      until (or (eq symbol-line :eof)
                (eq path-line   :eof)
                (null missing))
      for pos = (position symbol-line missing :test #'equal)
      when pos
      do (progn
           (delete symbol-line missing)
           (setf (gethash symbol-line found) path-line)))
    ; TODO(sterni): get rid of Data/../ in path
    (mapcar
      (lambda (x) (merge-pathnames x data-dir))
      (remove nil
        (mapcar (lambda (x) (gethash x found)) syms))))))

(defun main ()
  (let* ((browser (or (uiop:getenvp "BROWSER") "xdg-open"))
         (args    (#+sbcl uiop:command-line-arguments #+ecl ext:command-args))
         (prin    (member "--print" args :test #'equal))
         (syms    (remove-if (lambda (x) (eq (char x 0) #\-)) args))
         (paths (find-symbols-paths syms *clhs-path*)))
      (if (null paths)
        (uiop:quit 1)
        (dolist (p paths)
          (if prin
            (format t "~A~%" p)
            (uiop:launch-program
              (format nil "~A ~A" browser p)
              :force-shell t))))))
