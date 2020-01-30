;;; me-seconds.el --- How valuable is my time? -*- lexical-binding: t -*-
;; Author: William Carroll <wpcarro@gmail.com>

;;; Commentary:
;; Inspired by Google's concept of SWE-seconds, I decided to try and compute how
;; value my personal time is.
;;
;; This library should integrate with another library that handles currency
;; conversions using locally cached data for historial values and network
;; requests for current values.
;;
;; Context sensitivity:
;; Many of the values herein are based on my values that are a function of the
;; year, my current salary, my current company holiday policy, and my current
;; country holiday policy.  As such, many of these constants need to be updated
;; whenever changes occur in order for these functions to be useful.
;;
;; Units of time:
;; - seconds
;; - minutes
;; - hours
;; - days
;; - weeks
;; - months
;; - years
;;
;; Wish list:
;; - I should create a money.el struct to work with herein.  This module would
;;   expose basic algebra for working with money structs, which would be handy.
;; - I should create a time.el struct for working with hours in the day.  I'd
;;   like to be able to do (+ 9:15 17:45) cleanly.
;;
;; Terminology:
;; SWE hours give an order of magnitude approximation to the cost of resources
;; in dollars per hour at 2115 hours per year.
;; - SWE hour (SWEh)
;; - SWE year (SWEy)
;; - SWE nominal
;; - SWE opportunity
;;
;; Other isomorphisms include:
;; - Borg GCU
;; - Borg RAM
;; - Tape (library)
;; - Tape (vault)
;; - Spindles (low latency)
;; - Spindles (throughput)
;; - Spindles (throughput)
;; - Tape (throughput)
;; - SWE (nominal)
;; - SWE (opportunity)


;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dependencies
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'macros)
(require 'string)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun me-seconds/salary (amt)
  "Return the yearly rate of AMT of money in GBP.
f :: Integer -> Rate"
  (make-rate :money (make-money :whole amt :fractional 0 :currency 'GBP)
             :unit 'year))

(defconst me-seconds/salary (me-seconds/salary 80000)
  "My salary in GBP.")

;; TODO: Consider changing these into units of time.
(defconst me-seconds/months-per-year 12
  "Number of months in a year.")

(defconst me-seconds/days-per-year 365
  "Number of days in a year.")

(defconst me-seconds/hours-per-year (* 24 me-seconds/days-per-year)
  "Number of hours in a year.")

(defconst me-seconds/minutes-per-year (* 60 me-seconds/hours-per-year)
  "Number of minutes in a year.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Vacation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst me-seconds/bank-holidays-per-year 8
  "Number of bank holidays in the UK each year.")

(defconst me-seconds/pto-days-vacation-per-year 25
  "Number of days of paid-time-off I receive each year in the UK.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sleeping
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst me-seconds/sleeping-hours-per-day 8
  "An approximation of the number of hours I sleep each night on average.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Waking
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst me-seconds/waking-hours-per-day
  (- 24 me-seconds/sleeping-hours-per-night)
  "An approximation of the number of hours I sleep each night on average.")

;; TODO: Adjust this for vacation time.
(defconst me-seconds/waking-hours-per-year
  (* me-seconds/waking-hours-per-day me-seconds/days-per-year)
  "The number of hours that I work each year.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Working
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst me-seconds/working-hours-per-day
  (- 17 9)
  "An approximation of the number of hours I work each weekday on average.
Note that this differs from the assumed SWE hours per day calculation, which
  assumes 9 working hours.  See the discussion about this of go/rules-of-thumb.")

(defconst me-seconds/working-hours-per-year 2115
  "This number is borrowed from go/rules-of-thumb.")

;; Keep in mind that the following classifications of time:
;; - 9:00-17:00 M-F. Is this more expensive than time sleeping?
;; - Weekend
;; - Weekday
;; - Working hours
;; - Waking hours
;; - Sleeping hours
;; - Vacation hours
;;
;; TODO: Consider tax implications (i.e. after-tax amounts and pre-tax amounts).
;;
;; Should these all be treated the same since they all pull from the same pot of
;; time? Or perhaps there are multiples involved? Much to think about. How does
;; Google handle this?

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Library
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Supported currencies:
;; - GBP
;; NOTE: Amount is an integer.
(cl-defstruct money whole fractional currency)
(cl-defstruct rate money unit)

;; TODO: Add to money.el.
(defun money/to-string (x)
  "Return the string representation of X.
f :: Money -> String"
  (let ((currency (money-currency x))
        (whole (int-to-string (money-whole x)))
        (fract (int-to-string (money-fractional x))))
    (pcase currency
      ('GBP (string/concat "£" whole "." fract))
      ('USD (string/concat "$" whole "." fract))
      (_ (error (string/concat
                 "Currency: \""
                 (symbol-name currency)
                 "\" not supported"))))))

(macros/comment
 (money/to-string
  (make-money :whole 100 :fractional 99 :currency 'GBP)))

;; TODO: Add to rate.el.
(defun rate/to-string (x)
  "Message X as a rate.
f :: Rate -> String"
  (string/concat
   (money/to-string (rate-money x))
   " / "
   (pcase (rate-unit x)
     ('second "sec")
     ('minute "min")
     ('hour   "hr")
     ('day    "day")
     ('week   "week")
     ('month  "month")
     ('year   "year"))))

(macros/comment
 (rate/to-string
  (make-rate
   :money (make-money :whole 10 :fractional 10 :currency 'GBP)
   :unit 'day)))

;; TODO: Move this to math.el?
(defun ensure-float (x)
  "Ensures X is treated as a float."
  (+ 0.0 x))

;; TODO: Move these to basic time mapping module.
;; TODO: Consider making this an isomorphism.
(defun minutes/to-hours (x)
  "Convert X minutes to n hours."
  (/ x 60.0))

(defun hours/to-minutes (x)
  "Convert X hours to n minutes."
  (* x 60))

(defun days/to-minutes (x)
  "Convert X days to n minutes."
  (* x 24 60))

(defun weeks/to-minutes (x)
  "Convert X weeks to n minutes."
  (* x 7 24 60))

(defun months/to-minutes (x)
  "Convert X months to n minutes.
This approximates the number of days in a month to 30."
  (* x 30 24 60))

;; TODO: Support algebraic functions with money structs.
;; TODO: Support isomorphisms for rates to other units of time.  That would
;; subsume most of this module's use.
(defun me-seconds/value-per-minute (salary)
  "Computes my value per minute based on my current SALARY.
Signature: f :: Rate -> Rate
This is assuming that all of my time is equally valuable.  See the above
  discussion about the various classifications of my time.")

;; TODO: See note above about isomorphisms between various rates.
(defun me-seconds/value (salary x)
  "Compute the value of X minutes of my time at my current SALARY.
f :: Rate -> Integer -> Money")

(macros/comment
 (rate/to-string me-seconds/salary)
 )

(provide 'me-seconds)
;;; me-seconds.el ends here
