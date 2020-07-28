(in-package :panettone.tests)
(declaim (optimize (safety 3)))

(test initialize-issue-status-test
  (let ((issue (make-instance 'model:issue :status "open")))
    (is (eq :open (model:status issue)))))

(test initialize-issue-created-at-test
  (let* ((time (get-universal-time))
         (issue (make-instance 'model:issue :created-at time)))
    (is (local-time:timestamp=
         (local-time:universal-to-timestamp time)
         (model:created-at issue)))))
