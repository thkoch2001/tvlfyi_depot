;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dependencies
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'ert)
(require 'dash)
(require 'struct)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ert-deftest struct-set! ()
  (cl-defstruct dummy name age)
  (defvar struct--test-dummy (make-dummy :name "Roofus" :age 19))
  (struct-set! dummy name "Doofus" struct--test-dummy)
  (string= "Doofus" (dummy-name struct--test-dummy)))

(ert-deftest struct-set ()
  (cl-defstruct dummy name age)
  (defvar struct--test-dummy (make-dummy :name "Roofus" :age 19))
  (let ((result (struct-set dummy name "Shoofus" struct--test-dummy)))
    (and
     (string= "Roofus" (dummy-name struct--test-dummy))
     (string= "Shoofus" (dummy-name result)))))
