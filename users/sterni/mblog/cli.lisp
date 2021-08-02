(in-package :mblog)
(declaim (optimize (safety 3)))

(defparameter +synopsis+ "mnote-html FILE [FILE [ ... ]]")

;; TODO(sterni): handle relevant conditions
(defun main ()
  (let* ((args (uiop:command-line-arguments))
         (help-p (or (not args)
                     (find-if (lambda (x)
                                (member x '("-h" "--help" "--usage")
                                        :test #'string=))
                              args))))
    (if help-p (format *error-output* "Usage: ~A~%" +synopsis+)
      (loop for arg in args
            do (apple-note-html-fragment
                (mime:mime-message (pathname arg)) *standard-output*)))))
