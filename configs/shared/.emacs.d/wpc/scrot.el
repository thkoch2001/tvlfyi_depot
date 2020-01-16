;; Author: William Carroll <wpcarro@gmail.com>

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

(prelude/assert
 (prelude/executable-exists? "scrot"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Library
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst scrot/screenshot-directory "~/Downloads"
  "The default directory for screenshot outputs.")

(defconst scrot/path-to-executable "/usr/bin/scrot"
  "Path to the scrot executable.")

(defconst scrot/output-format "screenshot_%H:%M:%S_%Y-%m-%d.png"
  "The format string for the output screenshot file.
See scrot's man page for more information.")

(defun scrot/copy-image (path)
  "Use xclip to copy the image at PATH to the clipboard.
This currently only works for PNG files because that's what I'm outputting"
  (call-process "xclip" nil nil nil
                "-selection" "clipboard" "-t" "image/png" path)
  (message (string/format "[scrot.el] Image copied to clipboard!")))

(defmacro scrot/call (&rest args)
  "Call scrot with ARGS."
  `(call-process ,scrot/path-to-executable nil nil nil ,@args))

(defun scrot/fullscreen ()
  "Screenshot the entire screen."
  (interactive)
  (let ((screenshot-path (f-join scrot/screenshot-directory
                                 (ts-format scrot/output-format (ts-now)))))
    (scrot/call screenshot-path)
    (scrot/copy-image screenshot-path)))

(defun scrot/select ()
  "Click-and-drag to screenshot a region.
The output path is copied to the user's clipboard."
  (interactive)
  (let ((screenshot-path (f-join scrot/screenshot-directory
                                 (ts-format scrot/output-format (ts-now)))))
    (scrot/call "--select" screenshot-path)
    (scrot/copy-image screenshot-path)))

(exwm-input-set-key (kbd/raw 'x11 "s") #'scrot/select)

(provide 'scrot)
;;; scrot.el ends here
