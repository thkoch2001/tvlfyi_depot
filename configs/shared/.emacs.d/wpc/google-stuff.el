;;; google-stuff.el --- Working with Google infrastructure from Emacs -*- lexical-binding: t -*-
;; Author: William Carroll <wpcarro@gmail.com>

;;; Commentary:

;; First, I must opine.  Feel free to skip this section.  In general, it seems
;; that the typical programmer's workflow suffer from what economists call
;; "inelastic demand".  This means that any increase in the price of something
;; plummets the demand.  Another way of saying this is that programmers are
;; "price sensitive" when it comes to adopting new workflows.
;;
;; For us, any deviation from our "established" workflow feels costly.  This
;; makes sense to me because programming is already mentally taxing, so any
;; additional taxation can sometimes feel unbearable.  Until programming changes
;; dramatically and we relieve our dependence on files and text for modeling
;; complex applications, this price sensitivity will most likely remain the
;; status quo.  Therefore, it's critical to reduce the price of experimenting
;; with new tools such that new, superior workflows may emerge.  In this vain,
;; this module attempts to surface "luxury tools" (i.e. dependency pruners, code
;; linters, code formatters) via Emacs to reduce the price of experimenting with
;; them.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dependencies
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'f)
(require 'ivy-helpers)
(require 'maybe)
(require 'device)
(require 'macros)
(require 'general)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Library
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: Ensure a consistent and deliberate usage of `defvar', `defconst', and
;; `defcustom' across all Elisp modules.
(defcustom google-stuff/install-kbds? t
  "When t, install the keybindings defined herein.")

;; Definitions as explained by the highly knowledgeable Matthew (i.e. mjo@)
(defconst google-stuff/definitions
  '(
    ;; command-line tools
    ("gcert"  . "Requests a CorpSSH certificate.")
    ("glogin" . "SSO (i.e. Single Sign-On) cookie.")
    ("googlenetworkaccess" . "Device certificate that gives users a certificate
to access to the Google corp network.")
    ("prodaccess" . "Sets up a LOAS session on Goobuntu.")
    ;; general wtfs
    ("LOAS" . "Distributed authentication service used by jobs in production and
corp to authenticate each other. It's more efficient than SSL and works with
Stubby.")
    ))

(defconst google-stuff/tools
  '(("Depana" . "depana")
    ("Build cleaner" . "build_cleaner")
    ("Java formatter" . "google-java-format")
    ("Proto formatter" . "clang-format"))
  "Mapping of names of tools to the names of the executables that run them.")

(use-package protobuf-mode
  :config
  (macros/support-file-extension "pb" protobuf-mode))

;; TODO: Straighten out fig, citc, google3 and have modules for each.

;; TODO: Move this to a google3.el module.
(defconst google-stuff/root
  "/google/src/cloud/wpcarro"
  "The root directory to access google3.")

;; TODO: Find a fast way to generate this.
(defconst google-stuff/citc-clients
  '("auto-consult"
    "ac-skeleton")
  "A list of my active CitC clients.")


;; TODO: Can this be sourced from ~/.g4d?
(defconst google-stuff/citc-aliases
  '(("google3" . "/google3")
    ("escalations" . "/google3/corp/gtech/pto/tda/beacons_extension")
    ("spewall_fe" . "/google3/alkali/apps/speakeasydashboard")
    ("spewall_be" . "/google3/java/com/google/alkali/applications/speakeasydashboard")
    ("spewall_protos" . "/google3/google/internal/alkali/applications/speakeasydashboard")
    ("spewall_tests" . "/google3/javatests/com/google/alkali/applications/speakeasydashboard")
    ("gti" . "/google3/experimental/engedu/gti/projects/week20190422/mtv/Team10")
    ("authwf" . "/google3/customer_support/automation/workflow")
    ("redwood" . "/google3/customer_support/kms/redwood/ui")
    ("wf-fe" . "/google3/customer_support/kms/redwood/ui/client/components/item/workflow_editor")
    ("ac (alkali)" . "/google3/google/internal/alkali/applications/casesconsultservice")
    ("ac-server" . "/google3/java/com/google/alkali/applications/casesconsultservice/server/")
    ("ac-server (tests)" . "/google3/javatests/com/google/alkali/applications/casesconsultservice/server/"))
  "Mapping of a label to commonly visited locations in Google3.")


(defvar google-stuff/active-citc-client nil
  "Currently active CitC client.")

(defun google-stuff/depot-prefix ()
  "Return the current prefix for //depot/google3."
  (string/format "/google/src/cloud/wpcarro/%s/google3/"
                 google-stuff/active-citc-client))

(defun google-stuff/cs-url ()
  "Return the code-search URL for the current buffer and line number."
  (string/format "cs.corp.google.com/piper///depot/google3/%s?l=%s"
                 (s-chop-prefix
                  (google-stuff/depot-prefix)
                  (buffer-file-name))
                 (line-number-at-pos)))

(defun google-stuff/copy-cs-url ()
  "Copy the current file and line-position to the system clipboard."
  (interactive)
  (clipboard/copy (google-stuff/cs-url)))

(defun google-stuff/open-buffer-in-cs ()
  "Open the current file in Google's CodeSearch."
  (interactive)
  (shell-command
   (string/format "google-chrome '%s'"
                  (google-stuff/cs-url)
                  (line-number-at-pos))))

;; TODO: As a naming convention, should I prefer ivy or select? Or counsel?
(defun google-stuff/select-citc-client ()
  "Set `google-stuff/active-citc-client' with counsel."
  (interactive)
  (setq google-stuff/active-citc-client
        (ivy-read "CitC Client: " google-stuff/citc-clients)))

(defun google-stuff/remote-buffer? ()
  "Return t if buffer is one accessed via Tramp."
  (with-current-buffer (current-buffer)
    (if (file-remote-p default-directory)
        t
      nil)))

(defun google-stuff/jump-to-citc-alias ()
  "Use `find-file' to open an alias registered in `google-stuff/citc-aliases'.
When on a corporate laptop, remote connections are made using Tramp."
  (interactive)
  (when (maybe/nil? google-stuff/active-citc-client)
    (call-interactively #'google-stuff/select-citc-client))
  (ivy-helpers/kv
   "Jump to CitC Alias: "
   google-stuff/citc-aliases
   (lambda (k v)
     (->> v
          ;; If I don't remove the leading slash, `f-join' won't return a valid
          ;; path.
          (s-chop-prefix "/")
          (f-join google-stuff/root
                  google-stuff/active-citc-client)
          (s-prepend (if (device/work-laptop?) "/ssh:wpcarro@desktop:" ""))
          find-file))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Stuff I learned reading go/emacs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Fig
;; TODO: Make sure there are Evil-compatible KBDs for `fig-status'.
;; (require 'google-fig)

;; This allows `find-file' handle "//depot/google3/devtools/editors/".
(require 'p4-files)
(p4-enable-file-name-handler)

;; Blaze Support
;; - `google3-compile-current-file' is an excellent command!

;; google3-eglot (uses CiderLSP)
;; TODO: Make sure the functionality is supported as advertised:
;; - auto-completion
;; - eglot-help-at-point for documentation.
;; - goto-definition
;; - `eglot-code-actions' fixits
;; - `eglot-rename' refactoring
(require 'google3-eglot)
(google3-eglot-setup)

;; CodeSearch
;; TODO: Debug why this depends on google-piper and why I don't have that on my
;; desktop.
;; (require 'ivy-cs)

;; Auto completion
;; TODO: Is the part of or separate from google3-eglot?  Because google3-eglot
;; advertises auto-completion support.
(require 'google3-build-capf)
(google3-build-capf-enable-completions)
(add-to-list 'company-backends #'company-capf)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keybindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when google-stuff/install-kbds?
  (general-define-key
   :prefix "<SPC>"
   :states '(normal)
   "Gs" #'fig-status
   "Cs" #'google-stuff/open-buffer-in-cs
   "jc" #'google-stuff/jump-to-citc-alias))

(provide 'google-stuff)
;;; google-stuff.el ends here
