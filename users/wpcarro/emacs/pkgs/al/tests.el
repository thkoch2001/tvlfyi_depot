;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dependencies
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'ert)
(require 'al)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ert-deftest al-has-key? ()
  (and
   (al-has-key? 'fname '((fname . "William")))
   (not (al-has-key? 'lname '((fname . "William"))))))

(ert-deftest al-has-value? ()
  (and
   (al-has-value? "William" '((fname . "William")))
   (not (al-has-key? "John" '((fname . "William"))))))

(ert-deftest al-map-keys ()
  (equal '((2 . one)
           (3 . two))
         (al-map-keys #'1+
                      '((1 . one)
                        (2 . two)))))

(ert-deftest al-map-values ()
  (equal '((one . 2)
           (two . 3))
         (al-map-values #'1+
                        '((one . 1)
                          (two . 2)))))
