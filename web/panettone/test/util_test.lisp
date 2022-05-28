(in-package :panettone.tests)
(declaim (optimize (safety 3)))

(test add-missing-base64-padding-test
  (is (string=
       "abcdef"
       (base64:base64-string-to-string
        (panettone.util:add-missing-base64-padding
         "YWJjZGVm")))))
