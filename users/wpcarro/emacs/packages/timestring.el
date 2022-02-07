;;; timestring.el --- Quickly access timestamps in different formats -*- lexical-binding: t -*-

;; Author: William Carroll <wpcarro@gmail.com>
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1"))

;;; Commentary:

;; I was making some API calls where a URL needed a `since` parameter that of an
;; RFC 3339 encoded string.
;;
;; Because I didn't know what a RFC 3339 encoded
;; string was at the time, and because I didn't know what its format was
;; according to strftime, and because I'm most likely to forget both of these
;; things by the next time that I need something similar, I decided to write
;; this package so that I can accumulate a list of common time encodings.
;;
;; Thank you, Emacs.
;;
;; p.s. - I may turn this into a proper module and publish it.  But not today.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dependencies
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'ts)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Library
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgroup timestring nil
  "Customize group for timestring configuration.")

(defcustom timestring-supported-encodings
  '(("RFC 3339" . "%Y-%m-%dT%H:%M:%SZ")
    ;; Does anyone recognize this format?
    ("IDK" . "%Y-%m-%d %H:%M:%S %z"))
  "Mapping of encoding names to their format strings."
  :group 'timestring)

(defcustom timestring-supported-times
  '(("yesterday" . timestring--yesterday)
    ("now" . ts-now)
    ("tomorrow" . timestring--tomorrow))
  "Mapping of a labels to the functions that create those time objects."
  :group 'timestring)

(defun timestring--yesterday ()
  "Return a time object for yesterday."
  (ts-adjust 'day -1 (ts-now)))

(defun timestring--tomorrow ()
  "Return a time object for yesterday."
  (ts-adjust 'day +1 (ts-now)))

(defun timestring--completing-read (label xs)
  "Call `completing-read' with LABEL over the collection XS."
  (alist-get (completing-read label xs) xs nil nil #'equal))

(defun timestring-copy-encoded-time ()
  "Select a common time and an encoding.

The selected time will be encoded using the selected encoding and copied onto
your clipboard."
  (interactive)
  (let ((time (funcall (timestring--completing-read
                        "Time: " timestring-supported-times)))
        (fmt (timestring--completing-read
              "Encoding: " timestring-supported-encodings)))
    (kill-new (ts-format fmt time))
    (message "Copied!")))

(provide 'timestring)
;;; timestring.el ends here
