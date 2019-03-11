;; casing.el --- Helper functions for formatting text -*- lexical-binding: t -*-
;; Author: William Carroll <wpcarro@gmail.com>

;;; Commentary:
;; These functions are intended to be bound to KBDs for daily use and
;; refactoring.

;;; Code:

;; todo - grab the string at point and replace it with the output of
;; each fn

(defun caps->kebab (x)
  "Change the casing of X from CAP_CASE to kebab-case."
  (->> x
       s-downcase
       (s-replace "_" "-")))

(defun kebab->caps (x)
  "Change the casing of X from CAP_CASE to kebab-case."
  (->> x
       s-upcase
       (s-replace "-" "_")))

(defun lower->caps (x)
  "Change the casing of X from lowercase to CAPS_CASE."
  (->> x
       s-upcase
       (s-replace " " "_")))

(defun lower->kebab (x)
  "Change the casing of X from lowercase to kebab-case"
  (s-replace " " "-" x))

;;; Tests:

(ert-deftest caps->kebab-test ()
  (should (string= (caps->kebab "CAPS_CASE_STRING")
                   "caps-case-string")))

(ert-deftest kebab->caps-test ()
  (should (string= (kebab->caps "kebab-case-string")
                   "KEBAB_CASE_STRING")))

(provide 'casing)
;;; casing.el ends here
