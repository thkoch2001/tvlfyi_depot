;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dependencies
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'maybe)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ert-deftest maybe-nil? ()
  (should (maybe-nil? nil))
  (should (not (maybe-nil? t))))

(ert-deftest maybe-some? ()
  (should (maybe-some? '(1 2 3)))
  (should (not (maybe-some? nil))))

(ert-deftest maybe-default ()
  (should (string= "some" (maybe-default "some" nil)))
  (should (= 10 (maybe-default 1 10))))

(ert-deftest maybe-map ()
  (should (eq nil (maybe-map (lambda (x) (* x 2)) nil)))
  (should (= 4 (maybe-map (lambda (x) (* x 2)) 2))))
