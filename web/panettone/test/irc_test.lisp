(in-package :panettone.tests)
(declaim (optimize (safety 3)))

(test noping-test
  (is (not (equal "grfn" (panettone.irc:noping "grfn")))))
