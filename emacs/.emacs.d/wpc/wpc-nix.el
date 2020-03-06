;;; wpc-nix.el --- Nix support -*- lexical-binding: t -*-
;; Author: William Carroll <wpcarro@gmail.com>

;;; Commentary:
;; Configuration to support working with Nix.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dependencies
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: These may fail at startup. How can I make sure that the .envrc is
;; consulted when Emacs starts?
(prelude/assert (f-exists? (getenv "BRIEFCASE")))
(prelude/assert (f-exists? (getenv "DEPOT")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Library
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Code:
(use-package nix-mode
  :mode "\\.nix\\'")

;; TODO(wpcarro): Ensure the sub-process can resolve <briefcase>.
(defun nix/rebuild-emacs ()
  "Use nix-env to rebuild wpcarros-emacs."
  (interactive)
  (start-process "nix-build/<briefcase/emacs>" "*nix-build/<briefcase/emacs>*"
                 "nix-env" "-f" "<briefcase>" "-iA" "emacs")
  (display-buffer "*nix-build/<briefcase/emacs>*"))

(defun nix/home-manager-switch ()
  "Use Nix to reconfigure the user environment."
  (interactive)
  (start-process "nix/home-manager-switch" "*nix/home-manager-switch*"
                 "home-manager"
                 "-I" (format "nixpkgs=%s" (f-expand "~/nixpkgs"))
                 "-I" (format "home-manager=%s" (f-expand "~/home-manager"))
                 "switch")
  (display-buffer "*nix/home-manager-switch*"))

(defun nix/sly-from-briefcase (attribute)
  "Start a Sly REPL configured with a Lisp matching a derivation
  from my monorepo.

This function was taken from @tazjin's depot and adapted for my monorepo.

  The derivation invokes nix.buildLisp.sbclWith and is built
  asynchronously. The build output is included in the error
  thrown on build failures."
  (interactive "sAttribute: ")
  (lexical-let* ((outbuf (get-buffer-create (format "*briefcase-out/%s*" attribute)))
         (errbuf (get-buffer-create (format "*briefcase-errors/%s*" attribute)))
         (expression (format "let depot = import <depot> {}; briefcase = import <briefcase> {}; in depot.nix.buildLisp.sbclWith [ briefcase.%s ]" attribute))
         (command (list "nix-build" "-E" expression)))
    (message "Acquiring Lisp for <briefcase>.%s" attribute)
    (make-process :name (format "nix-build/%s" attribute)
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
                             (message "Acquired Lisp for <briefcase>.%s at %s" attribute lisp-path)
                             (sly lisp-path)))
                          (_ (with-current-buffer errbuf
                               (error "Failed to build '%s':\n%s" attribute (buffer-string)))))
                      (kill-buffer outbuf)
                      (kill-buffer errbuf))))))

(provide 'wpc-nix)
;;; wpc-nix.el ends here
