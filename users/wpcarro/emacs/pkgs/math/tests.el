;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dependencies
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'ert)
(require 'math)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ert-deftest math-mod ()
  (should (= 0 (math-mod 9 3)))
  (should (= 4 (math-mod 9 5))))

(ert-deftest math-exp ()
  (should (= 9 (math-exp 3 2)))
  (should (= 8 (math-exp 2 3))))

(ert-deftest math-round ()
  (should (= 10 (math-round 9.5)))
  (should (= 9 (math-round 9.45))))

(ert-deftest math-floor ()
  (should (= 9 (math-floor 9.5))))
