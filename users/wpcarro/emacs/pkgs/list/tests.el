;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dependencies
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'ert)
(require 'list)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ert-deftest list-length ()
  (= 0 (list-length '()))
  (= 5 (list-length '(1 2 3 4 5))))

(ert-deftest list-reduce ()
  (= 16 (list-reduce 1 (lambda (x acc) (+ x acc)) '(1 2 3 4 5))))

(ert-deftest list-map ()
  (equal '(2 4 6 8 10)
         (list-map (lambda (x) (* x 2)) '(1 2 3 4 5))))

(ert-deftest list-xs-distinct-by? ()
  (list-xs-distinct-by?
   (lambda (x) (plist-get x :kbd))
     '((:kbd "C-a" [:name] "foo")

       (:kbd "C-b" :name "[]foo"))))

(ert-deftest list-dedupe-adjacent ()
  (equal '(1 2 3 4 3 5)
         (list-dedupe-adjacent '(1 1 1 2 2 3 4 4 3 5 5))))
