;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dependencies
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'ert)
(require 'bag)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq fixture (bag-from-list '(1 1 1 2 2 3)))

(ert-deftest bag-add ()
  (should (not (bag-contains? 4 fixture)))
  (should (bag-contains? 4 (bag-add 4 fixture))))

(ert-deftest bag-remove ()
  (should (bag-contains? 1 fixture))
  (should (not (bag-contains? 3 (bag-remove 3 fixture)))))

(ert-deftest bag-count ()
  (should (= 3 (bag-count 1 fixture)))
  (should (= 2 (bag-count 2 fixture)))
  (should (= 1 (bag-count 3 fixture))))

(ert-deftest bag-total ()
  (should (= 6 (bag-total fixture))))

(ert-deftest bag-contains? ()
  (should (bag-contains? 1 fixture))
  (should (not (bag-contains? 4 fixture))))
