;;; dired.el --- My dired preferences -*- lexical-binding: t -*-
;; Author: William Carroll <wpcarro@gmail.com>

;;; Commentary:
;; File management in Emacs, if learned and configured properly, should be
;; capable to reduce my dependency on the terminal.

;;; Code:

;; TODO: Ensure sorting in dired is by type.

;; TODO: Rename wpc-dired.el to file-management.el

(progn
  (require 'dired)
  (setq dired-recursive-copies 'always
        dired-recursive-deletes 'top
        dired-dwim-target t)
  (general-define-key
   :keymaps 'dired-mode-map
   :states 'normal
   "s" nil
   "q" (lambda () (interactive) (kill-buffer nil))
   "c" #'find-file
   "f" #'wpc/find-file
   "-" (lambda () (interactive) (find-alternate-file "..")))
  (general-add-hook 'dired-mode-hook
                    (list (enable dired-hide-details-mode)
                          #'auto-revert-mode)))

(progn
  (require 'locate)
  (general-define-key
   :keymaps 'locate-mode-map
   :states 'normal
   "o" #'dired-display-file))

(provide 'wpc-dired)
;;; wpc-dired.el ends here
