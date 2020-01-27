;;; wpc-nix.el --- Nix support -*- lexical-binding: t -*-
;; Author: William Carroll <wpcarro@gmail.com>

;;; Commentary:
;; Configuration to support working with Nix.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dependencies
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(prelude/assert (f-exists? "~/universe"))
(prelude/assert (f-exists? "~/depot"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Library
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Code:
(use-package nix-mode
  :mode "\\.nix\\'")

(defun nix/sly-from-universe (attribute)
  "Start a Sly REPL configured with a Lisp matching a derivation
  from my monorepo.

This function was taken from @tazjin's depot and adapted for my monorepo.

  The derivation invokes nix.buildLisp.sbclWith and is built
  asynchronously. The build output is included in the error
  thrown on build failures."
  (interactive "sAttribute: ")
  (lexical-let* ((outbuf (get-buffer-create (format "*universe-out/%s*" attribute)))
         (errbuf (get-buffer-create (format "*universe-errors/%s*" attribute)))
         (expression (format "let depot = import <depot> {}; universe = import <universe> {}; in depot.nix.buildLisp.sbclWith [ universe.%s ]" attribute))
         (command (list "nix-build" "-E" expression)))
    (message "Acquiring Lisp for <depot>.%s" attribute)
    (make-process :name (format "depot-nix-build/%s" attribute)
                  :buffer outbuf
                  :stderr errbuf
                  :command command
                  :sentinel
                  (lambda (process event)
                    (unwind-protect
                        (pcase event
                          ("finished\n"
                           (let* ((outpath (s-trim (with-current-buffer outbuf (buffer-string))))
                                  (lisp-path (s-concat outpath "/bin/sbcl")))
                             (message "Acquired Lisp for <depot>.%s at %s" attribute lisp-path)
                             (sly lisp-path)))
                          (_ (with-current-buffer errbuf
                               (error "Failed to build '%s':\n%s" attribute (buffer-string)))))
                      (kill-buffer outbuf)
                      (kill-buffer errbuf))))))

(provide 'wpc-nix)
;;; wpc-nix.el ends here
