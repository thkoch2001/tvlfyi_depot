;;; nix-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "nix" "nix.el" (23430 41818 774263 912000))
;;; Generated autoloads from nix.el

(autoload 'pcomplete/nix "nix" "\
Completion for the nix command.

\(fn)" nil nil)

;;;***

;;;### (autoloads nil "nix-drv-mode" "nix-drv-mode.el" (23430 41818
;;;;;;  763128 639000))
;;; Generated autoloads from nix-drv-mode.el

(autoload 'nix-drv-mode "nix-drv-mode" "\
Pretty print Nix’s .drv files.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "nix-mode" "nix-mode.el" (23430 41818 769555
;;;;;;  1000))
;;; Generated autoloads from nix-mode.el

(autoload 'nix-indent-line "nix-mode" "\
Indent current line in a Nix expression.

\(fn)" t nil)

(autoload 'nix-mode "nix-mode" "\
Major mode for editing Nix expressions.

The following commands may be useful:

  '\\[newline-and-indent]'
    Insert a newline and move the cursor to align with the previous
    non-empty line.

  '\\[fill-paragraph]'
    Refill a paragraph so that all lines are at most `fill-column'
    lines long.  This should do the right thing for comments beginning
    with `#'.  However, this command doesn't work properly yet if the
    comment is adjacent to code (i.e., no intervening empty lines).
    In that case, select the text to be refilled and use
    `\\[fill-region]' instead.

The hook `nix-mode-hook' is run when Nix mode is started.

\\{nix-mode-map}

\(fn)" t nil)

;;;***

;;;### (autoloads nil "nix-prettify-mode" "nix-prettify-mode.el"
;;;;;;  (23430 41818 767901 837000))
;;; Generated autoloads from nix-prettify-mode.el

(autoload 'nix-prettify-mode "nix-prettify-mode" "\
Toggle Nix Prettify mode.

With a prefix argument ARG, enable Nix Prettify mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil.

When Nix Prettify mode is enabled, hash-parts of the Nix store
file names (see `nix-prettify-regexp') are prettified,
i.e. displayed as `nix-prettify-char' character.  This mode can
be enabled programmatically using hooks:

  (add-hook 'shell-mode-hook 'nix-prettify-mode)

It is possible to enable the mode in any buffer, however not any
buffer's highlighting may survive after adding new elements to
`font-lock-keywords' (see `nix-prettify-special-modes' for
details).

Also you can use `global-nix-prettify-mode' to enable Nix
Prettify mode for all modes that support font-locking.

\(fn &optional ARG)" t nil)

(defvar nix-prettify-global-mode nil "\
Non-nil if Nix-Prettify-Global mode is enabled.
See the `nix-prettify-global-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `nix-prettify-global-mode'.")

(custom-autoload 'nix-prettify-global-mode "nix-prettify-mode" nil)

(autoload 'nix-prettify-global-mode "nix-prettify-mode" "\
Toggle Nix-Prettify mode in all buffers.
With prefix ARG, enable Nix-Prettify-Global mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Nix-Prettify mode is enabled in all buffers where
`nix-prettify-turn-on' would do it.
See `nix-prettify-mode' for more information on Nix-Prettify mode.

\(fn &optional ARG)" t nil)

(define-obsolete-function-alias 'global-nix-prettify-mode 'nix-prettify-global-mode)

;;;***

;;;### (autoloads nil "nix-repl" "nix-repl.el" (23430 41818 771127
;;;;;;  391000))
;;; Generated autoloads from nix-repl.el

(autoload 'nix-repl "nix-repl" "\
Load the Nix-REPL.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "nix-search" "nix-search.el" (23430 41818 761247
;;;;;;  123000))
;;; Generated autoloads from nix-search.el

(autoload 'nix-search "nix-search" "\
Run nix search.
SEARCH a search term to use.
FILE a Nix expression to search in.

\(fn &optional SEARCH FILE)" t nil)

;;;***

;;;### (autoloads nil "nix-shell" "nix-shell.el" (23430 41818 778535
;;;;;;  603000))
;;; Generated autoloads from nix-shell.el

(autoload 'nix-shell-unpack "nix-shell" "\
Run Nix’s unpackPhase.
FILE is the file to unpack from.
ATTR is the attribute to unpack.

\(fn FILE ATTR)" t nil)

(autoload 'nix-shell-configure "nix-shell" "\
Run Nix’s configurePhase.
FILE is the file to configure from.
ATTR is the attribute to configure.

\(fn FILE ATTR)" t nil)

(autoload 'nix-shell-build "nix-shell" "\
Run Nix’s buildPhase.
FILE is the file to build from.
ATTR is the attribute to build.

\(fn FILE ATTR)" t nil)

(autoload 'nix-shell-with-string "nix-shell" "\
A nix-shell emulator in Emacs from a string.
STRING the nix expression to use.

\(fn STRING)" nil nil)

(autoload 'nix-shell "nix-shell" "\
A nix-shell emulator in Emacs.
FILE the file to instantiate.
ATTR an attribute of the Nix file to use.

\(fn FILE &optional ATTR)" t nil)

;;;***

;;;### (autoloads nil nil ("nix-build.el" "nix-edit.el" "nix-format.el"
;;;;;;  "nix-instantiate.el" "nix-log.el" "nix-mode-pkg.el" "nix-shebang.el"
;;;;;;  "nix-store.el") (23430 41818 781257 265000))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; nix-mode-autoloads.el ends here
