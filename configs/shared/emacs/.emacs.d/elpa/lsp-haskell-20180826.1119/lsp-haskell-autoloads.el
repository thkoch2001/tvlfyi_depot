;;; lsp-haskell-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "lsp-haskell" "lsp-haskell.el" (23428 20523
;;;;;;  947450 33000))
;;; Generated autoloads from lsp-haskell.el

(let ((loads (get 'lsp-haskell 'custom-loads))) (if (member '"lsp-haskell" loads) nil (put 'lsp-haskell 'custom-loads (cons '"lsp-haskell" loads))))

(defvar lsp-haskell-process-path-hie "hie-wrapper" "\
The path for starting the haskell-ide-engine
server. hie-wrapper exists on HIE master from 2018-06-10")

(custom-autoload 'lsp-haskell-process-path-hie "lsp-haskell" t)

(defvar lsp-haskell-process-args-hie '("-d" "-l" "/tmp/hie.log") "\
The arguments for starting the haskell-ide-engine server.
For a debug log, use `-d -l /tmp/hie.log'.")

(custom-autoload 'lsp-haskell-process-args-hie "lsp-haskell" t)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; lsp-haskell-autoloads.el ends here
