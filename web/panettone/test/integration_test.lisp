(in-package :panettone.tests)
(declaim (optimize (safety 3)))

(defparameter run-integration-tests?
  (uiop:getenvp "RUN_INTEGRATION_TESTS"))

(defmacro integration-test (name &body body)
  `(test ,name
     (if run-integration-tests?
         ,@body
         (skip "Not running integration tests"))))

(defvar *port* nil
  "The port that the panettone test server is running on")
(def-fixture panettone-server ()
  (let* ((hunchentoot:*catch-errors-p* nil)
         (ldap-host (or (uiop:getenvp "LDAP_HOST") "localhost"))
         (ldap-port (integer-env "LDAP_PORT"))
         (_ (progn
              (when (null postmodern:*database*)
                (panettone.model:connect-postgres
                 :database "panettone_test"))
              (panettone:migrate-db)))
         (acceptor (hunchentoot:start
                    (make-instance 'easy-routes:routes-acceptor :port 0)))
         (*port* (hunchentoot:acceptor-port acceptor)))
    (&body)
    (hunchentoot:stop acceptor)))

(defun server-request (path &rest args)
  (apply
   #'drakma:http-request
   (format nil
           "http://127.0.0.1:~A~A~A"
           *port*
           (if (string-equal path "/" :end1 1) "" "/")
           path)
   args))

(integration-test list-issues-test
  (with-fixture panettone-server ()
    (multiple-value-bind (_resp status _headers) (server-request "/")
      (is (= status 200)))))

(comment
 (setq run-integration-tests? t)
 (run-all-tests)

 (fiveam:run!)
 )
