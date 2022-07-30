;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dependencies
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'ert)
(require 'al)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ert-deftest al-has-key? ()
  (should (al-has-key? 'fname '((fname . "William"))))
  (should (not (al-has-key? 'lname '((fname . "William"))))))

(ert-deftest al-get ()
  (let ((xs (->> (al-new)
                 (al-set 'fname "John")
                 (al-set 'employed? nil))))
    (should (string= "John" (al-get 'fname xs)))
    (should (string= "Cleese" (al-get 'lname xs "Cleese")))
    ;; Test that the value of nil is returned even when a default is defined,
    ;; which could be a subtle bug in the typical Elisp pattern of supporting
    ;; defaults with: (or foo default).
    (should (eq nil (al-get 'employed? xs)))
    (should (eq nil (al-get 'employed? xs "default")))))

(ert-deftest al-has-value? ()
  (should (al-has-value? "William" '((fname . "William"))))
  (should (not (al-has-key? "John" '((fname . "William"))))))

(ert-deftest al-map-keys ()
  (should
   (equal '((2 . one)
            (3 . two))
          (al-map-keys #'1+
                       '((1 . one)
                         (2 . two))))))

(ert-deftest al-map-values ()
  (should (equal '((one . 2)
                   (two . 3))
                 (al-map-values #'1+
                                '((one . 1)
                                  (two . 2))))))

(ert-deftest al-delete ()
  (let ((person (->> (al-new)
                     (al-set "fname" "John")
                     (al-set "lname" "Cleese")
                     (al-set "age" 82))))
    (should (al-has-key? "age" person))
    (should (not (al-has-key? "age" (al-delete "age" person))))))
