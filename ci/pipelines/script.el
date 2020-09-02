;; This script initializes Emacs and exits with either a zero or non-zero status
;; depending on whether or not Emacs initialized without logging warnings or
;; encountering errors.
;;
;; This script reads the location of init.el as the last argument in `argv'.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dependencies
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'prelude)
(require 'f)
(require 'dash)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Script
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar init-el-path (-last-item argv)
  "Path to the init.el file that this script attempts to load.")

(prelude-assert (f-exists? init-el-path))

(condition-case err
    (load init-el-path)
  (error
   (message "Encountered an error while attempting to load init.el: %s" err)
   (kill-emacs 1)))

(when (bufferp "*Errors*")
  (progn
    (with-current-buffer "*Errors*"
      (message "Encountered errors in *Errors* buffer: %s" (buffer-string)))
    (kill-emacs 1)))

(when (bufferp "*Warnings*")
  (progn
    (with-current-buffer "*Warnings*"
      (message "Encountered warnings in *Warnings* buffer: %s" (buffer-string)))
    (kill-emacs 1)))

(message "Successfully init'd Emacs without encountering errors or warnings!")
(kill-emacs 0)
