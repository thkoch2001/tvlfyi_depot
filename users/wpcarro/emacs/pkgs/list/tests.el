;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dependencies
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'ert)
(require 'list)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq xs '(1 2 3 4 5))

(ert-deftest list-length ()
  (should (= 0 (list-length '())))
  (should (= 5 (list-length xs))))

(ert-deftest list-reduce ()
  (should (= 16 (list-reduce 1 (lambda (x acc) (+ x acc)) xs))))

(ert-deftest list-map ()
  (should
   (equal '(2 4 6 8 10)
          (list-map (lambda (x) (* x 2)) xs))))

(ert-deftest list-xs-distinct-by? ()
  (should
   (equal t (list-xs-distinct-by?
             (lambda (x) (plist-get x :kbd))
             '((:kbd "C-a" :name "foo")
               (:kbd "C-b" :name "foo"))))))

(ert-deftest list-dedupe-adjacent ()
  (should (equal '(1 2 3 4 3 5)
                 (list-dedupe-adjacent '(1 1 1 2 2 3 4 4 3 5 5)))))

(ert-deftest list-contains? ()
  ;; Assert returns t or nil
  (should (equal t (list-contains? 1 xs)))
  (should (equal nil (list-contains? 100 xs))))

(ert-deftest list-join ()
  (should (equal "foo-bar-baz"
                 (list-join "-" '("foo" "bar" "baz")))))
