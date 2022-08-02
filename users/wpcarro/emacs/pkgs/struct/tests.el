;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dependencies
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'ert)
(require 'struct)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl-defstruct dummy name age)

(ert-deftest struct-update ()
  (let* ((test (make-dummy :name "Roofus" :age 19))
         (result (struct-update dummy name #'upcase test)))
    ;; test
    (should (string= "Roofus" (dummy-name test)))
    (should (= 19 (dummy-age test)))
    ;; result
    (should (string= "ROOFUS" (dummy-name result)))
    (should (= 19 (dummy-age result)))))

(ert-deftest struct-update! ()
  (let ((test (make-dummy :name "Roofus" :age 19)))
    (struct-update! dummy name #'upcase test)
    (should (string= "ROOFUS" (dummy-name test)))
    (should (= 19 (dummy-age test)))))

(ert-deftest struct-set ()
  (let* ((test (make-dummy :name "Roofus" :age 19))
         (result (struct-set dummy name "Shoofus" test)))
    ;; test
    (should (string= "Roofus" (dummy-name test)))
    (should (= 19 (dummy-age test)))
    ;; result
    (should (string= "Shoofus" (dummy-name result)))
    (should (= 19 (dummy-age result)))))

(ert-deftest struct-set! ()
  (let ((test (make-dummy :name "Roofus" :age 19)))
    (struct-set! dummy name "Doofus" test)
    (should (string= "Doofus" (dummy-name test)))
    (should (= 19 (dummy-age test)))))
