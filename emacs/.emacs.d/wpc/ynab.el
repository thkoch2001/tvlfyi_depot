;;; ynab.el --- Functions for YNAB's API -*- lexical-binding: t -*-
;; Author: William Carroll <wpcarro@gmail.com>

;;; Commentary:
;; I'm not sure what the outcome of this project is.  I'm just writing some
;; Elisp at the moment to document some of my cursory interactions with YNAB's
;; API.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dependencies
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'json)
(require 'a)
(require 'request)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Library
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar ynab/api-url "https://api.youneedabudget.com/v1/"
  "The URL of the YNAB API.")

(defun ynab/get-secret (name)
  "Fetch and decrypt the secret for YNAB at NAME in the password store."
  (password-store-get (format "%s/%s" "finance/youneedabudget.com" name)))

(defvar ynab/personal-access-token
  (ynab/get-secret "personal-access-token")
  "My personal access token to YNAB's API.")

(defvar ynab/budget-id
  (ynab/get-secret "budget-id")
  "The ID of my current budget on YNAB.")

(defvar ynab/account-id
  (ynab/get-secret "account-id")
  "The ID of my current budget on YNAB.")

(defun ynab/url-for-endpoint (endpoint)
  "Return the URL for the YNAB ENDPOINT.
This will resolve any variables in the form of {variable_name} using a prefined
scope object."
  (format "%s%s" ynab/api-url endpoint))

(macros/comment
 ;; TODO: Use these this map to resolve variables in an endpoint URL like
 ;; '/budgets/{budget_id}/'.
 '((budget_id . (ynab/get-secret "budget-id"))
   (account_id . (ynab/get-secret "account-id")))
 (request (ynab/url-for-endpoint "/budgets/{budget_id}/transactions")))

(provide 'ynab)
;;; ynab.el ends here
