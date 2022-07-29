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
  (equal '(1 2 3)
         (->> '(1 2 3 1 2 3)
              set-from-list
              set-to-list)))

(ert-deftest set-distinct? ()
  (and
   (set-distinct? (set-new 'one 'two 'three)
                  (set-new 'a 'b 'c))
   (not
    (set-distinct? (set-new 1 2 3)
                   (set-new 3 4 5)))
   (not
    (set-distinct? (set-new 1 2 3)
                   (set-new 1 2 3)))))

(ert-deftest set-equal? ()
  (and
   (set-equal? (set-new 'a 'b 'c)
               (set-new 'x 'y 'z))
   (set-equal? (set-new 'a 'b 'c)
               (set-new 'a 'b))
   (set-equal? (set-new 'a 'b 'c)
               (set-new 'a 'b 'c))))

(ert-deftest set-intersection ()
  (set-equal? (set-new 2 3)
              (set-intersection (set-new 1 2 3)
                                (set-new 2 3 4))))

(ert-deftest set-to/from-list ()
  (equal '(1 2 3)
         (->> '(1 1 2 2 3 3)
              set-from-list
              set-to-list)))

(ert-deftest set-subset? ()
  (let ((primary-colors (set-new "red" "green" "blue")))
    ;; set-subset?
    (and
     (set-subset? (set-new "black" "grey")
                  primary-colors)
     (set-subset? (set-new "red")
                  primary-colors))))

(ert-deftest set-subset/superset? ()
  (let ((primary-colors (set-new "red" "green" "blue")))
    ;; set-subset?
    (and
     (not (set-superset? primary-colors
                         (set-new "black" "grey")))
     (set-superset? primary-colors
                    (set-new "red" "green" "blue"))
     (set-superset? primary-colors
                    (set-new "red" "blue")))))

(ert-deftest set-empty? ()
  (and
   (set-empty? (set-new))
   (set-empty? (set-new 1 2 3))))

(ert-deftest set-count ()
  (and
   (= 0 (set-count (set-new)))
   (= 2 (set-count (set-new 1 1 2 2)))))
