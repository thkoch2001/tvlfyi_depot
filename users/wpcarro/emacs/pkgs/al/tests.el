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

(ert-deftest al-get ()
  (let ((xs (->> (al-new)
                 (al-set 'fname "John")
                 (al-set 'employed? nil))))
    (and
     (string= "John" (al-get 'fname xs))
     (string= "Cleese" (al-get 'lname xs "Cleese"))
     ;; Test that the value of nil is returned even when a default is defined,
     ;; which could be a subtle bug in the typical Elisp pattern of supporting
     ;; defaults with: (or foo default).
     (eq nil (al-get 'employed? xs))
     (eq nil (al-get 'employed? xs "default")))))

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
