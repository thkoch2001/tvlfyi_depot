;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dependencies
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'ert)
(require 'bytes)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ert-deftest bytes-to-string ()
  (should (equal "1000B" (bytes-to-string 1000)))
  (should (equal "2KB" (bytes-to-string (* 2 bytes-kb))))
  (should (equal "17MB" (bytes-to-string (* 17 bytes-mb))))
  (should (equal "419GB" (bytes-to-string (* 419 bytes-gb))))
  (should (equal "999TB" (bytes-to-string (* 999 bytes-tb))))
  (should (equal "2PB" (bytes-to-string (* 2 bytes-pb)))))
