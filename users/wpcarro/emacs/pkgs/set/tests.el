;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dependencies
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'ert)
(require 'dash)
(require 'set)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ert-deftest set-from-list ()
  (should (equal '(1 2 3)
                 (->> '(1 2 3 1 2 3)
                      set-from-list
                      set-to-list))))

(ert-deftest set-distinct? ()
  (should (set-distinct? (set-new 'one 'two 'three)
                         (set-new 'a 'b 'c)))
  (should (not
           (set-distinct? (set-new 1 2 3)
                          (set-new 3 4 5))))
  (should (not
           (set-distinct? (set-new 1 2 3)
                          (set-new 1 2 3)))))

(ert-deftest set-equal? ()
  (should (not (set-equal? (set-new 'a 'b 'c)
                           (set-new 'x 'y 'z))))
  (should (not (set-equal? (set-new 'a 'b 'c)
                           (set-new 'a 'b))))
  (should (set-equal? (set-new 'a 'b 'c)
                      (set-new 'a 'b 'c))))

(ert-deftest set-intersection ()
  (should (set-equal? (set-new 2 3)
                      (set-intersection (set-new 1 2 3)
                                        (set-new 2 3 4)))))

(ert-deftest set-to/from-list ()
  (should (equal '(1 2 3)
                 (->> '(1 1 2 2 3 3)
                      set-from-list
                      set-to-list))))

(ert-deftest set-subset? ()
  (should (not (set-subset? (set-new "black" "grey")
                            (set-new "red" "green" "blue"))))
  (should (set-subset? (set-new "red")
                       (set-new "red" "green" "blue"))))

(ert-deftest set-superset? ()
  (let ((primary-colors (set-new "red" "green" "blue")))
    (should (not (set-superset? primary-colors
                                (set-new "black" "grey"))))
    (should (set-superset? primary-colors
                           (set-new "red" "green" "blue")))
    (should (set-superset? primary-colors
                           (set-new "red" "blue")))))

(ert-deftest set-empty? ()
  (should (set-empty? (set-new)))
  (should (not (set-empty? (set-new 1 2 3)))))

(ert-deftest set-count ()
  (should (= 0 (set-count (set-new))))
  (should (= 2 (set-count (set-new 1 1 2 2)))))
