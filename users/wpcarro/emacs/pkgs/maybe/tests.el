;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dependencies
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'maybe)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ert-deftest maybe-nil? ()
  (and
   (maybe-nil? nil)
   (not (maybe-nil? t))))

(ert-deftest maybe-some? ()
  (and
   (maybe-some? '(1 2 3))
   (not (maybe-some? nil))))

(ert-deftest maybe-default ()
  (and
   (string= "some" (maybe-default "some" nil))
   (= 10 (maybe-default 1 10))))

(ert-deftest maybe-map ()
  (eq nil (maybe-map (lambda (x) (* x 2)) nil))
  (= 4 (maybe-map (lambda (x) (* x 2)) 2)))
