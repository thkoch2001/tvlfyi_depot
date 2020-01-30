;;; bills.el --- Helping me manage my bills -*- lexical-binding: t -*-
;; Author: William Carroll <wpcarro@gmail.com>

;;; Commentary:
;; For personal use only.

;;; Code:

(defconst bills/whitelist '(("Council Tax" . "rbkc.gov.uk/onlinepayments/counciltaxpayments/")
                            ("Internet". "plus.net/member-centre/login"))
  "Maps searchable labels to URLs to pay these bills.")

(defun bills/url ()
  "Copies the URL to pay a bill onto the clipboard."
  (ivy-read
   "Bill: "
   bills/whitelist
   :action (lambda (entry)
             (kill-new (cdr entry))
             (alert "Copied to clipboard!"))))

(macros/comment
 (bills/url))

(provide 'bills)
;;; bills.el ends here
