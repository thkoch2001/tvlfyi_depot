;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dependencies
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'ert)
(require 'symbol)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ert-deftest symbol-to-string ()
  (should (string= "foo" (symbol-to-string 'foo))))

(ert-deftest symbol-from-string ()
  (should (eq 'foo (symbol-from-string "foo"))))

(ert-deftest symbol-as-string ()
  (should (eq 'foo-hook
              (symbol-as-string
               (lambda (x) (format "%s-hook" x))
               'foo))))
