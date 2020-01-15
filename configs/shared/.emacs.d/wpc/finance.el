;;; finance.el --- Functions to help me organize my finances -*- lexical-binding: t -*-
;; Author: William Carroll <wpcarro@gmail.com>

;;; Commentary:
;; Using functions to organize my financial thinking.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dependencies
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'prelude)
(require 'math)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Library
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar finance/enable-tests? t
  "When t, run the tests defined herein.")

;; TODO: Support printing an org-table of these amount in a similar format to:
;; https://keisan.casio.com/exec/system/1234231998
(cl-defun finance/future-value (amt
                                &key
                                num-years
                                (frequency 'monthly)
                                (interest-rate 0.06)
                                (payment-due-at 'beg)
                                (present-value 0))
  "Compute the Future Value of AMT.

This function assumes that the interest rate is applied annually and not
monthly.

This function will attempt to provide the following defaults:
- frequency: 'monthly
- interest-rate: 6%
- payment-due-at: 'beg
- present-value: 0.00"
  (prelude/assert (set/contains? payment-due-at (set/new 'beg 'end)))
  (prelude/assert (set/contains? frequency (set/new 'annually
                                                    'semiannually
                                                    'quarterly
                                                    'monthly)))
  (let ((pmt amt)
        (k (alist/get frequency '((annually . 1)
                                  (semiannually . 2)
                                  (quarterly . 4)
                                  (monthly . 12))))
        (r interest-rate)
        (n num-years)
        (pv present-value))
    (if (= 0 r)
        (+ pv (* pmt n k))
      (if (equal 'beg payment-due-at)
          (+ (* pv (math/exp (+ 1 (/ r k)) (* n k)))
             (* pmt
                (/ (- (math/exp (+ 1 (/ r k)) (* n k)) 1)
                   (/ r k))
                (+ 1 (/ r k))))
        (+ (* pv (math/exp (+ 1 (/ r k)) (* n k)))
           (* pmt
              (/ (- (math/exp (+ 1 (/ r k)) (* n k)) 1)
                 (/ r k))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when finance/enable-tests?
  (prelude/assert
   (equal "1551.27"
          (string/format "%0.2f"
                         (finance/future-value
                          9.99
                          :interest-rate 0.05
                          :num-years 10
                          :frequency 'monthly
                          :payment-due-at 'end
                          :present-value 0))))
  (prelude/assert
   (equal "14318.34"
          (string/format "%0.2f"
                         (finance/future-value 10.0 :num-years 35))))
  (prelude/assert
   (equal "4200.00"
          (string/format "%0.2f"
                         (finance/future-value
                          10.0
                          :interest-rate 0.0
                          :num-years 35
                          :frequency 'monthly
                          :payment-due-at 'beg
                          :present-value 0))))
  (prelude/assert
   (equal "14318.34"
          (string/format "%0.2f"
                         (finance/future-value
                          10.0
                          :interest-rate 0.06
                          :num-years 35
                          :frequency 'monthly
                          :payment-due-at 'beg
                          :present-value 0))))
  (prelude/assert
   (equal "38282.77"
          (string/format "%0.2f"
                         (finance/future-value
                          10.0
                          :interest-rate 0.1
                          :num-years 35
                          :frequency 'monthly
                          :payment-due-at 'beg
                          :present-value 0)))))

(provide 'finance)
;;; finance.el ends here
