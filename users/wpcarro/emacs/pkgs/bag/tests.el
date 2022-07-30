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
  (and
   (not (bag-contains? 4 fixture))
   (bag-contains? 4 (bag-add 4 fixture))))

(ert-deftest bag-remove ()
  (and
   (bag-contains? 1 fixture)
   (not (bag-contains? 3 (bag-remove 3 fixture)))))

(ert-deftest bag-count ()
  (and
   (= 3 (bag-count 1 fixture))
   (= 2 (bag-count 2 fixture))
   (= 1 (bag-count 3 fixture))))

(ert-deftest bag-total ()
  (= 6 (bag-total fixture)))

(ert-deftest bag-contains? ()
  (and
   (bag-contains? 1 fixture)
   (not (bag-contains? 4 fixture))))
