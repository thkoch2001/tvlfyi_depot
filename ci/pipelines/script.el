;; This script initializes Emacs and exits with either a zero or non-zero status
;; depending on whether or not Emacs initialized without logging warnings or
;; encountering errors.
;;
;; This script reads the location of init.el as the last argument in `argv'.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dependencies
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'dash)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Script
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(condition-case err
    (load (-last-item argv))
  (error
   (message "Encountered an error while attempting to load init.el: %s" err)
   (kill-emacs 1)))

(if (bufferp "*Warnings*")
    (progn
      (with-current-buffer "*Warnings*"
        (message "Encountered warnings in *Warnings* buffer: %s" (buffer-string)))
      (kill-emacs 1))
  (kill-emacs 0))
