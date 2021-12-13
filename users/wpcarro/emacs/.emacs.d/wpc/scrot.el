;;; scrot.el --- Screenshot functions -*- lexical-binding: t -*-

;; Author: William Carroll <wpcarro@gmail.com>
;; Version: 0.0.1
;; URL: https://git.wpcarro.dev/wpcarro/briefcase
;; Package-Requires: ((emacs "24"))

;;; Commentary:
;; scrot is a Linux utility for taking screenshots.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dependencies
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'f)
(require 'string)
(require 'ts)
(require 'clipboard)
(require 'kbd)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Library
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst scrot-screenshot-directory "~/Downloads"
  "The default directory for screenshot outputs.")

(defconst scrot-path-to-executable "/usr/bin/scrot"
  "Path to the scrot executable.")

(defconst scrot-output-format "screenshot_%H:%M:%S_%Y-%m-%d.png"
  "The format string for the output screenshot file.
See scrot's man page for more information.")

(defun scrot--copy-image (path)
  "Use xclip to copy the image at PATH to the clipboard.
This currently only works for PNG files because that's what I'm outputting"
  (call-process "xclip" nil nil nil
                "-selection" "clipboard" "-t" "image/png" path)
  (message (string-format "[scrot.el] Image copied to clipboard!")))

(defun scrot-select ()
  "Click-and-drag to screenshot a region.
The output path is copied to the user's clipboard."
  (interactive)
  (let ((screenshot-path (f-join scrot-screenshot-directory
                                 (ts-format scrot-output-format (ts-now)))))
    (make-process
     :name "scrot-select"
     :command `(,scrot-path-to-executable "--select" ,screenshot-path)
     :sentinel (lambda (proc _err)
                 (when (= 0 (process-exit-status proc))
                   (scrot--copy-image screenshot-path))))))

(provide 'scrot)
;;; scrot.el ends here
