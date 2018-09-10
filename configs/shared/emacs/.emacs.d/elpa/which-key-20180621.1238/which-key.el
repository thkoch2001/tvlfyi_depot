;;; which-key.el --- Display available keybindings in popup  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Free Software Foundation, Inc.

;; Author: Justin Burkett <justin@burkett.cc>
;; Maintainer: Justin Burkett <justin@burkett.cc>
;; URL: https://github.com/justbur/emacs-which-key
;; Package-Version: 20180621.1238
;; Version: 3.3.0
;; Keywords:
;; Package-Requires: ((emacs "24.4"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; which-key provides the minor mode which-key-mode for Emacs. The mode displays
;; the key bindings following your currently entered incomplete command (a
;; prefix) in a popup. For example, after enabling the minor mode if you enter
;; C-x and wait for the default of 1 second the minibuffer will expand with all
;; of the available key bindings that follow C-x (or as many as space allows
;; given your settings). This includes prefixes like C-x 8 which are shown in a
;; different face. Screenshots of what the popup will look like along with
;; information about additional features can be found at
;; https://github.com/justbur/emacs-which-key.
;;

;;; Code:

(require 'cl-lib)
(require 'button)
(require 'regexp-opt)

;; For compiler
(defvar evil-operator-shortcut-map)
(defvar evil-operator-state-map)
(defvar evil-motion-state-map)
(defvar golden-ratio-mode)
(declare-function evil-get-command-property "ext:evil-common.el")

;;; Options

(defgroup which-key nil
  "Customization options for which-key-mode"
  :group 'help
  :prefix "which-key-")

(defcustom which-key-idle-delay 1.0
  "Delay (in seconds) for which-key buffer to popup. A value of zero
might lead to issues, so a non-zero value is recommended
(see https://github.com/justbur/emacs-which-key/issues/134)."
  :group 'which-key
  :type 'float)

(defcustom which-key-idle-secondary-delay nil
  "Once the which-key buffer shows once for a key sequence reduce
the idle time to this amount (in seconds). This makes it possible
to shorten the delay for subsequent popups in the same key
sequence. The default is for this value to be nil, which disables
this behavior."
  :group 'which-key
  :type '(choice float (const :tag "Disabled" nil)))

(defcustom which-key-echo-keystrokes (if (and echo-keystrokes
                                              (> (+ echo-keystrokes 0.01)
                                                 which-key-idle-delay))
                                         (/ (float which-key-idle-delay) 4)
                                       echo-keystrokes)
  "Value to use for `echo-keystrokes'.
This only applies if `which-key-popup-type' is minibuffer or
`which-key-show-prefix' is echo. It needs to be less than
`which-key-idle-delay' or else the keystroke echo will erase the
which-key popup."
  :group 'which-key
  :type 'float)

(defcustom which-key-max-description-length 27
  "Truncate the description of keys to this length.
Also adds \"..\". If nil, disable any truncation."
  :group 'which-key
  :type '(choice integer (const :tag "Disable truncation" nil)))

(defcustom which-key-add-column-padding 0
  "Additional padding (number of spaces) to add to the left of
each key column."
  :group 'which-key
  :type 'integer)

(defcustom which-key-unicode-correction 3
  "Correction for wide unicode characters.
Since we measure width in terms of the number of characters,
Unicode characters that are wider than ASCII characters throw off
the calculation for available width in the which-key buffer.  This
variable allows you to adjust for the wide unicode characters by
artificially reducing the available width in the buffer.

The default of 3 means allow for the total extra width
contributed by any wide unicode characters to be up to one
additional ASCII character in the which-key buffer.  Increase this
number if you are seeing charaters get cutoff on the right side
of the which-key popup."
  :group 'which-key
  :type 'integer)

(defcustom which-key-dont-use-unicode nil
  "If non-nil, don't use any unicode characters in default setup."
  :group 'which-key
  :type 'boolean)

(defcustom which-key-separator
  (if which-key-dont-use-unicode " : " " → ")
  "Separator to use between key and description. Default is \" →
\", unless `which-key-dont-use-unicode' is non nil, in which case
the default is \" : \"."
  :group 'which-key
  :type 'string)

(defcustom which-key-prefix-prefix "+"
  "String to insert in front of prefix commands (i.e., commands
that represent a sub-map). Default is \"+\"."
  :group 'which-key
  :type 'string)

(defcustom which-key-compute-remaps nil
  "If non-nil, show remapped command if a command has been
remapped given the currently active keymaps."
  :group 'which-key
  :type 'boolean)

(defvar which-key-key-replacement-alist nil)
(make-obsolete-variable 'which-key-key-replacement-alist
                        'which-key-replacement-alist "2016-11-21")
(defvar which-key-description-replacement-alist nil)
(make-obsolete-variable 'which-key-description-replacement-alist
                        'which-key-replacement-alist "2016-11-21")
(defvar which-key-key-based-description-replacement-alist nil)
(make-obsolete-variable 'which-key-key-based-description-replacement-alist
                        'which-key-replacement-alist "2016-11-21")

(defcustom which-key-replacement-alist
  (delq nil
        `(((nil . "Prefix Command") . (nil . "prefix"))
          ((nil . "\\`\\?\\?\\'") . (nil . "lambda"))
          ((nil . "which-key-show-next-page-no-cycle") . (nil . "wk next pg"))
          ,@(unless which-key-dont-use-unicode
              '((("<left>") . ("←"))
                (("<right>") . ("→"))))
          (("<\\([[:alnum:]-]+\\)>") . ("\\1"))))
  "Association list to determine how to manipulate descriptions
of key bindings in the which-key popup. Each element of the list
is a nested cons cell with the format

\(MATCH CONS . REPLACEMENT\).

The MATCH CONS determines when a replacement should occur and
REPLACEMENT determines how the replacement should occur. Each may
have the format \(KEY REGEXP . BINDING REGEXP\). For the
replacement to apply the key binding must match both the KEY
REGEXP and the BINDING REGEXP. A value of nil in either position
can be used to match every possibility. The replacement is
performed by using `replace-regexp-in-string' on the KEY REGEXP
from the MATCH CONS and REPLACEMENT when it is a cons cell, and
then similarly for the BINDING REGEXP. A nil value in the BINDING
REGEXP position cancels the replacement. For example, the entry

\(\(nil . \"Prefix Command\"\) . \(nil . \"prefix\"\)\)

matches any binding with the descriptions \"Prefix Command\" and
replaces the description with \"prefix\", ignoring the
corresponding key.

REPLACEMENT may also be a function taking a cons cell
\(KEY . BINDING\) and producing a new corresponding cons cell.

If REPLACEMENT is anything other than a cons cell \(and non nil\)
the key binding is ignored by which-key.

Finally, you can multiple replacements to occur for a given key
binding by setting `which-key-allow-multiple-replacements' to a
non-nil value."
  :group 'which-key
  :type '(alist :key-type (cons (choice regexp (const nil))
                                (choice regexp (const nil)))
                :value-type (cons (choice string (const nil))
                                  (choice string (const nil)))))

(when (bound-and-true-p which-key-key-replacement-alist)
  (mapc
   (lambda (repl)
     (push (cons (cons (car repl) nil) (cons (cdr repl) nil))
           which-key-replacement-alist))
   which-key-key-replacement-alist))
(when (bound-and-true-p which-key-description-replacement-alist)
  (mapc
   (lambda (repl)
     (push (cons (cons nil (car repl)) (cons nil (cdr repl)))
           which-key-replacement-alist))
   which-key-description-replacement-alist))

(defcustom which-key-allow-multiple-replacements nil
  "Allow a key binding to match and be modified by multiple
elements in `which-key-replacement-alist' if non-nil. When nil,
only the first match is used to perform replacements from
`which-key-replacement-alist'."
  :group 'which-key
  :type 'boolean)

(defcustom which-key-show-docstrings nil
  "If non-nil, show each command's docstring next to the command
in the which-key buffer. This will only display the docstring up
to the first line break. If you set this variable to the symbol
docstring-only, then the command's name with be omitted. You
probably also want to adjust `which-key-max-description-length'
at the same time if you use this feature."
  :group 'which-key
  :type '(radio
          (const :tag "Do not show docstrings" nil)
          (const :tag "Add docstring to command names" t)
          (const :tag "Replace command name with docstring" docstring-only)))

(defcustom which-key-highlighted-command-list '()
  "A list of strings and/or cons cells used to highlight certain
commands. If the element is a string, assume it is a regexp
pattern for matching command names and use
`which-key-highlighted-command-face' for any matching names. If
the element is a cons cell, it should take the form (regexp .
face to apply)."
  :group 'which-key
  :type  '(repeat (choice string (cons regexp face))))

(defcustom which-key-special-keys '()
  "These keys will automatically be truncated to one character
and have `which-key-special-key-face' applied to them. This is
disabled by default. Try this to see the effect.

\(setq which-key-special-keys '(\"SPC\" \"TAB\" \"RET\" \"ESC\" \"DEL\")\)"
  :group 'which-key
  :type '(repeat string))

(defcustom which-key-buffer-name " *which-key*"
  "Name of which-key buffer."
  :group 'which-key
  :type 'string)

(defcustom which-key-show-prefix 'echo
  "Whether to and where to display the current prefix sequence.
Possible choices are echo for echo area (the default), left, top
and nil. Nil turns the feature off."
  :group 'which-key
  :type '(radio (const :tag "Left of the keys" left)
                (const :tag "In the first line" top)
                (const :tag "In the last line" bottom)
                (const :tag "In the echo area" echo)
                (const :tag "In the mode-line" mode-line)
                (const :tag "Hide" nil)))

(defcustom which-key-popup-type 'side-window
  "Supported types are minibuffer, side-window, frame, and custom."
  :group 'which-key
  :type '(radio (const :tag "Show in minibuffer" minibuffer)
                (const :tag "Show in side window" side-window)
                (const :tag "Show in popup frame" frame)
                (const :tag "Use your custom display functions" custom)))

(defcustom which-key-min-display-lines 1
  "The minimum number of horizontal lines to display in the
  which-key buffer."
  :group 'which-key
  :type 'integer)

(defcustom which-key-max-display-columns nil
  "The maximum number of columns to display in the which-key
buffer. nil means don't impose a maximum."
  :group 'which-key
  :type '(choice integer (const :tag "Unbounded" nil)))

(defcustom which-key-side-window-location 'bottom
  "Location of which-key popup when `which-key-popup-type' is side-window.
Should be one of top, bottom, left or right. You can also specify
a list of two locations, like (right bottom). In this case, the
first location is tried. If there is not enough room, the second
location is tried."
  :group 'which-key
  :type '(radio (const right)
                (const bottom)
                (const left)
                (const top)
                (const (right bottom))
                (const (bottom right))))

(defcustom which-key-side-window-slot 0
  "The `slot' to use for `display-buffer-in-side-window' when
`which-key-popup-type' is 'side-window. Quoting from the
docstring of `display-buffer-in-side-window',

‘slot’ if non-nil, specifies the window slot where to display
  BUFFER.  A value of zero or nil means use the middle slot on
  the specified side.  A negative value means use a slot
  preceding (that is, above or on the left of) the middle slot.
  A positive value means use a slot following (that is, below or
  on the right of) the middle slot.  The default is zero."
  :group 'which-key
  :type 'integer)

(defcustom which-key-side-window-max-width 0.333
  "Maximum width of which-key popup when type is side-window and
location is left or right.
This variable can also be a number between 0 and 1. In that case, it denotes
a percentage out of the frame's width."
  :group 'which-key
  :type 'float)

(defcustom which-key-side-window-max-height 0.25
  "Maximum height of which-key popup when type is side-window and
location is top or bottom.
This variable can also be a number between 0 and 1. In that case, it denotes
a percentage out of the frame's height."
  :group 'which-key
  :type 'float)

(defcustom which-key-frame-max-width 60
  "Maximum width of which-key popup when type is frame."
  :group 'which-key
  :type 'integer)

(defcustom which-key-frame-max-height 20
  "Maximum height of which-key popup when type is frame."
  :group 'which-key
  :type 'integer)

(defcustom which-key-allow-imprecise-window-fit nil
  "If non-nil allow which-key to use a less intensive method of
fitting the popup window to the buffer. If you are noticing lag
when the which-key popup displays turning this on may help.

See https://github.com/justbur/emacs-which-key/issues/130"
  :group 'which-key
  :type 'boolean)

(defcustom which-key-show-remaining-keys nil
  "Show remaining keys in last slot, when keys are hidden."
  :group 'which-key
  :type '(radio (const :tag "Yes" t)
                (const :tag "No" nil)))

(defcustom which-key-sort-order 'which-key-key-order
  "If nil, do not resort the output from
`describe-buffer-bindings' which groups by mode. Ordering options
are

1. `which-key-key-order': by key (default)
2. `which-key-key-order-alpha': by key using alphabetical order
3. `which-key-description-order': by description
4. `which-key-prefix-then-key-order': prefix (no prefix first) then key
5. `which-key-local-then-key-order': local binding then key

See the README and the docstrings for those functions for more
information."
  :group 'which-key
  :type '(choice (function-item which-key-key-order)
                 (function-item which-key-key-order-alpha)
                 (function-item which-key-description-order)
                 (function-item which-key-prefix-then-key-order)
                 (function-item which-key-local-then-key-order)))

(defcustom which-key-sort-uppercase-first t
  "If non-nil, uppercase comes before lowercase in sorting
function chosen in `which-key-sort-order'. Otherwise, the order
is reversed."
  :group 'which-key
  :type 'boolean)

(defcustom which-key-paging-prefixes '()
  "Enable paging for these prefixes."
  :group 'which-key
  :type '(repeat string))

(defcustom which-key-paging-key "<f5>"
  "Key to use for changing pages. Bound after each of the
prefixes in `which-key-paging-prefixes'"
  :group 'which-key
  :type 'string)

;; (defcustom which-key-undo-key nil
;;   "Key (string) to use for undoing keypresses. Bound recursively
;; in each of the maps in `which-key-undo-keymaps'."
;;   :group 'which-key
;;   :type 'string)

;; (defcustom which-key-undo-keymaps '()
;;   "Keymaps in which to bind `which-key-undo-key'"
;;   :group 'which-key
;;   :type '(repeat symbol))

(defcustom which-key-use-C-h-commands t
  "Use C-h for paging if non-nil. Normally C-h after a prefix
  calls `describe-prefix-bindings'. This changes that command to
  a which-key paging command when which-key-mode is active."
  :group 'which-key
  :type 'boolean)

(defcustom which-key-is-verbose nil
  "Whether to warn about potential mistakes in configuration."
  :group 'which-key
  :type 'boolean)

(defvar which-key-C-h-map
  (let ((map (make-sparse-keymap)))
    (dolist (bind '(("\C-a" . which-key-abort)
                    ("a" . which-key-abort)
                    ("\C-d" . which-key-toggle-docstrings)
                    ("d" . which-key-toggle-docstrings)
                    ("\C-h" . which-key-show-standard-help)
                    ("h" . which-key-show-standard-help)
                    ("\C-n" . which-key-show-next-page-cycle)
                    ("n" . which-key-show-next-page-cycle)
                    ("\C-p" . which-key-show-previous-page-cycle)
                    ("p" . which-key-show-previous-page-cycle)
                    ("\C-u" . which-key-undo-key)
                    ("u" . which-key-undo-key)
                    ("1" . which-key-digit-argument)
                    ("2" . which-key-digit-argument)
                    ("3" . which-key-digit-argument)
                    ("4" . which-key-digit-argument)
                    ("5" . which-key-digit-argument)
                    ("6" . which-key-digit-argument)
                    ("7" . which-key-digit-argument)
                    ("8" . which-key-digit-argument)
                    ("9" . which-key-digit-argument)))
      (define-key map (car bind) (cdr bind)))
    map)
  "Keymap for C-h commands.")

(defvar which-key--paging-functions '(which-key-C-h-dispatch
                                      which-key-turn-page
                                      which-key-show-next-page-cycle
                                      which-key-show-next-page-no-cycle
                                      which-key-show-previous-page-cycle
                                      which-key-show-previous-page-no-cycle
                                      which-key-undo-key
                                      which-key-undo))

(defcustom which-key-hide-alt-key-translations t
  "Hide key translations using Alt key if non nil.
These translations are not relevant most of the times since a lot
of terminals issue META modifier for the Alt key.

See http://www.gnu.org/software/emacs/manual/html_node/emacs/Modifier-Keys.html"
  :group 'which-key
  :type 'boolean)

(defcustom which-key-delay-functions nil
  "A list of functions that may decide whether to delay the
which-key popup based on the current incomplete key
sequence. Each function in the list is run with two arguments,
the current key sequence as produced by `key-description' and the
length of the key sequence. If the popup should be delayed based
on that key sequence, the function should return the delay time
in seconds. Returning nil means no delay. The first function in
this list to return a value is the value that is used.

The delay time is effectively added to the normal
`which-key-idle-delay'."
  :group 'which-key
  :type '(repeat function))

(defcustom which-key-allow-regexps nil
  "A list of regexp strings to use to filter key sequences. When
non-nil, for a key sequence to trigger the which-key popup it
must match one of the regexps in this list. The format of the key
sequences is what is produced by `key-description'."
  :group 'which-key
  :type '(repeat regexp))

(defcustom which-key-inhibit-regexps nil
  "Similar to `which-key-allow-regexps', a list of regexp strings
to use to filter key sequences. When non-nil, for a key sequence
to trigger the which-key popup it cannot match one of the regexps
in this list. The format of the key sequences is what is produced
by `key-description'."
  :group 'which-key
  :type '(repeat regexp))

(defcustom which-key-show-transient-maps nil
  "Show keymaps created by `set-transient-map' when applicable.

More specifically, detect when `overriding-terminal-local-map' is
set (this is the keymap used by `set-transient-map') and display
it."
  :group 'which-key
  :type 'boolean)

(defcustom which-key-enable-extended-define-key nil
  "Advise `define-key' to make which-key aware of definitions of the form

  \(define-key KEYMAP KEY '(\"DESCRIPTION\" . DEF))

With the advice, this definition will have the side effect of
creating a replacement in `which-key-replacement-alist' that
replaces DEF with DESCRIPTION when the key sequence ends in
KEY. Using a cons cell like this is a valid definition for
`define-key'. All this does is to make which-key aware of it.

Since many higher level keybinding functions use `define-key'
internally, this will affect most if not all of those as well.

This variable must be set before loading which-key."
  :group 'which-key
  :type 'boolean)

;; Hooks
(defcustom which-key-init-buffer-hook '()
  "Hook run when which-key buffer is initialized."
  :group 'which-key
  :type 'hook)

;;;; Faces

(defgroup which-key-faces nil
  "Faces for which-key-mode"
  :group 'which-key
  :prefix "which-key-")

(defface which-key-key-face
  '((t . (:inherit font-lock-constant-face)))
  "Face for which-key keys"
  :group 'which-key-faces)

(defface which-key-separator-face
  '((t . (:inherit font-lock-comment-face)))
  "Face for the separator (default separator is an arrow)"
  :group 'which-key-faces)

(defface which-key-note-face
  '((t . (:inherit which-key-separator-face)))
  "Face for notes or hints occasionally provided"
  :group 'which-key-faces)

(defface which-key-command-description-face
  '((t . (:inherit font-lock-function-name-face)))
  "Face for the key description when it is a command"
  :group 'which-key-faces)

(defface which-key-local-map-description-face
  '((t . (:inherit which-key-command-description-face)))
  "Face for the key description when it is found in `current-local-map'"
  :group 'which-key-faces)

(defface which-key-highlighted-command-face
  '((t . (:inherit which-key-command-description-face :underline t)))
  "Default face for the command description when it is a command
and it matches a string in `which-key-highlighted-command-list'."
  :group 'which-key-faces)

(defface which-key-group-description-face
  '((t . (:inherit font-lock-keyword-face)))
  "Face for the key description when it is a group or prefix"
  :group 'which-key-faces)

(defface which-key-special-key-face
  '((t . (:inherit which-key-key-face :inverse-video t :weight bold)))
  "Face for special keys (SPC, TAB, RET)"
  :group 'which-key-faces)

(defface which-key-docstring-face
  '((t . (:inherit which-key-note-face)))
  "Face for docstrings"
  :group 'which-key-faces)

;;;; Custom popup

(defcustom which-key-custom-popup-max-dimensions-function nil
  "Variable to hold a custom max-dimensions function.
Will be passed the width of the active window and is expected to
return the maximum height in lines and width in characters of the
which-key popup in the form a cons cell (height . width)."
  :group 'which-key
  :type '(choice function (const nil)))

(defcustom which-key-custom-hide-popup-function nil
  "Variable to hold a custom hide-popup function.
It takes no arguments and the return value is ignored."
  :group 'which-key
  :type '(choice function (const nil)))

(defcustom which-key-custom-show-popup-function nil
  "Variable to hold a custom show-popup function.
Will be passed the required dimensions in the form (height .
width) in lines and characters respectively.  The return value is
ignored."
  :group 'which-key
  :type '(choice function (const nil)))

(defcustom which-key-lighter " WK"
  "Minor mode lighter to use in the mode-line."
  :group 'which-key
  :type 'string)

(defvar which-key-inhibit nil
  "Prevent which-key from popping up momentarily by setting this
to a non-nil value for the execution of a command. Like this

\(let \(\(which-key-inhibit t\)\)
...\)")

(defvar which-key-keymap-history nil
  "History of keymap selections in functions like
`which-key-show-keymap'.")

;;; Internal Vars

(defvar which-key--buffer nil
  "Internal: Holds reference to which-key buffer.")
(defvar which-key--timer nil
  "Internal: Holds reference to open window timer.")
(defvar which-key--secondary-timer-active nil
  "Internal: Non-nil if the secondary timer is active.")
(defvar which-key--paging-timer nil
  "Internal: Holds reference to timer for paging.")
(defvar which-key--frame nil
  "Internal: Holds reference to which-key frame.
Used when `which-key-popup-type' is frame.")
(defvar which-key--echo-keystrokes-backup nil
  "Internal: Backup the initial value of `echo-keystrokes'.")
(defvar which-key--prefix-help-cmd-backup nil
  "Internal: Backup the value of `prefix-help-command'.")
(defvar which-key--last-try-2-loc nil
  "Internal: Last location of side-window when two locations
used.")
(defvar which-key--automatic-display nil
  "Internal: Non-nil if popup was triggered with automatic
update.")
(defvar which-key--multiple-locations nil)
(defvar which-key--inhibit-next-operator-popup nil)
(defvar which-key--prior-show-keymap-args nil)
(defvar which-key--previous-frame-size nil)
(defvar which-key--prefix-title-alist nil)
(defvar which-key--debug nil)
(defvar which-key--evil-keys-regexp (eval-when-compile
                                      (regexp-opt '("-state"))))
(defvar which-key--ignore-non-evil-keys-regexp
  (eval-when-compile
    (regexp-opt '("mouse-" "wheel-" "remap" "drag-" "scroll-bar"
                  "select-window" "switch-frame" "which-key-"))))
(defvar which-key--ignore-keys-regexp
  (eval-when-compile
    (regexp-opt '("mouse-" "wheel-" "remap" "drag-" "scroll-bar"
                  "select-window" "switch-frame" "-state"
                  "which-key-"))))

(make-obsolete-variable 'which-key-prefix-name-alist nil "2016-10-05")
(make-obsolete-variable 'which-key-prefix-title-alist nil "2016-10-05")

(defvar which-key--pages-obj nil)
(cl-defstruct which-key--pages
  pages
  height
  widths
  keys/page
  page-nums
  num-pages
  total-keys
  prefix
  prefix-title)

(defun which-key--rotate (list n)
  (let* ((len (length list))
         (n (if (< n 0) (+ len n) n))
         (n (mod n len)))
    (append (last list (- len n)) (butlast list (- len n)))))

(defun which-key--pages-set-current-page (pages-obj n)
  (setf (which-key--pages-pages pages-obj)
        (which-key--rotate (which-key--pages-pages pages-obj) n))
  (setf (which-key--pages-widths pages-obj)
        (which-key--rotate (which-key--pages-widths pages-obj) n))
  (setf (which-key--pages-keys/page pages-obj)
        (which-key--rotate (which-key--pages-keys/page pages-obj) n))
  (setf (which-key--pages-page-nums pages-obj)
        (which-key--rotate (which-key--pages-page-nums pages-obj) n))
  pages-obj)

(defsubst which-key--on-first-page ()
  (= (which-key--pages-page-nums which-key--pages-obj) 1))

(defsubst which-key--on-last-page ()
  (= (which-key--pages-page-nums which-key--pages-obj)
     (which-key--pages-num-pages which-key--pages-obj)))

(defsubst which-key--current-prefix ()
  (when which-key--pages-obj
    (which-key--pages-prefix which-key--pages-obj)))

;;; Third-party library support
;;;; Evil

(defcustom which-key-allow-evil-operators (boundp 'evil-this-operator)
  "Allow popup to show for evil operators. The popup is normally
  inhibited in the middle of commands, but setting this to
  non-nil will override this behavior for evil operators."
  :group 'which-key
  :type 'boolean)

(defcustom which-key-show-operator-state-maps nil
  "Experimental: Try to show the right keys following an evil
command that reads a motion, such as \"y\", \"d\" and \"c\" from
normal state. This is experimental, because there might be some
valid keys missing and it might be showing some invalid keys."
  :group 'which-key
  :type 'boolean)

;;;;; God-mode

(defvar which-key--god-mode-support-enabled nil
  "Support god-mode if non-nil. This is experimental,
so you need to explicitly opt-in for now. Please report any
problems at github.")

(defvar which-key--god-mode-key-string nil
  "Holds key string to use for god-mode support.")

(defadvice god-mode-lookup-command
    (around which-key--god-mode-lookup-command-advice disable)
  (setq which-key--god-mode-key-string (ad-get-arg 0))
  (unwind-protect
      ad-do-it
    (when (bound-and-true-p which-key-mode)
      (which-key--hide-popup))))

(defun which-key-enable-god-mode-support (&optional disable)
  "Enable support for god-mode if non-nil. This is experimental,
so you need to explicitly opt-in for now. Please report any
problems at github. If DISABLE is non-nil disable support."
  (interactive "P")
  (setq which-key--god-mode-support-enabled (null disable))
  (if disable
      (ad-disable-advice
       'god-mode-lookup-command
       'around 'which-key--god-mode-lookup-command-advice)
    (ad-enable-advice
     'god-mode-lookup-command
     'around 'which-key--god-mode-lookup-command-advice))
  (ad-activate 'god-mode-lookup-command))

;;; Mode

;;;###autoload
(define-minor-mode which-key-mode
  "Toggle which-key-mode."
  :global t
  :lighter which-key-lighter
  :keymap (let ((map (make-sparse-keymap)))
            (mapc
             (lambda (prefix)
               (define-key map
                 (kbd (concat prefix " " which-key-paging-key))
                 #'which-key-C-h-dispatch))
             which-key-paging-prefixes)
            map)
  (if which-key-mode
      (progn
        (setq which-key--echo-keystrokes-backup echo-keystrokes)
        (when (or (eq which-key-show-prefix 'echo)
                  (eq which-key-popup-type 'minibuffer))
          (which-key--setup-echo-keystrokes))
        (unless (member prefix-help-command which-key--paging-functions)
          (setq which-key--prefix-help-cmd-backup prefix-help-command))
        (when which-key-use-C-h-commands
          (setq prefix-help-command #'which-key-C-h-dispatch))
        (when which-key-show-remaining-keys
          (add-hook 'pre-command-hook #'which-key--lighter-restore))
        (add-hook 'pre-command-hook #'which-key--hide-popup)
        (add-hook 'focus-out-hook #'which-key--stop-timer)
        (add-hook 'focus-in-hook #'which-key--start-timer)
        (add-hook 'window-size-change-functions
                  'which-key--hide-popup-on-frame-size-change)
        (which-key--start-timer))
    (setq echo-keystrokes which-key--echo-keystrokes-backup)
    (when which-key--prefix-help-cmd-backup
      (setq prefix-help-command which-key--prefix-help-cmd-backup))
    (when which-key-show-remaining-keys
      (remove-hook 'pre-command-hook #'which-key--lighter-restore))
    (remove-hook 'pre-command-hook #'which-key--hide-popup)
    (remove-hook 'focus-out-hook #'which-key--stop-timer)
    (remove-hook 'focus-in-hook #'which-key--start-timer)
    (remove-hook 'window-size-change-functions
                 'which-key--hide-popup-on-frame-size-change)
    (which-key--stop-timer)))

(defun which-key--init-buffer ()
  "Initialize which-key buffer"
  (unless (buffer-live-p which-key--buffer)
    (setq which-key--buffer (get-buffer-create which-key-buffer-name))
    (with-current-buffer which-key--buffer
      ;; suppress confusing minibuffer message
      (let (message-log-max)
        (toggle-truncate-lines 1)
        (message ""))
      (setq-local cursor-type nil)
      (setq-local cursor-in-non-selected-windows nil)
      (setq-local mode-line-format nil)
      (setq-local word-wrap nil)
      (setq-local show-trailing-whitespace nil)
      (run-hooks 'which-key-init-buffer-hook))))

(defun which-key--setup-echo-keystrokes ()
  "Reduce `echo-keystrokes' if necessary (it will interfere if
it's set too high)."
  (when (and echo-keystrokes
             (> (abs (- echo-keystrokes which-key-echo-keystrokes)) 0.000001))
    (if (> which-key-idle-delay which-key-echo-keystrokes)
        (setq echo-keystrokes which-key-echo-keystrokes)
      (setq which-key-echo-keystrokes (/ (float which-key-idle-delay) 4)
            echo-keystrokes which-key-echo-keystrokes))))

(defun which-key-remove-default-unicode-chars ()
  "Use of `which-key-dont-use-unicode' is preferred to this
function, but it's included here in case someone cannot set that
variable early enough in their configuration, if they are using a
starter kit for example."
  (when (string-equal which-key-separator " → ")
    (setq which-key-separator " : "))
  (setq which-key-key-replacement-alist
        (delete '("left" . "←") which-key-key-replacement-alist))
  (setq which-key-key-replacement-alist
        (delete '("right" . "→") which-key-key-replacement-alist)))

;;; Default configuration functions for use by users.

;;;###autoload
(defun which-key-setup-side-window-right ()
  "Apply suggested settings for side-window that opens on right."
  (interactive)
  (setq which-key-popup-type 'side-window
        which-key-side-window-location 'right
        which-key-show-prefix 'top))

;;;###autoload
(defun which-key-setup-side-window-right-bottom ()
  "Apply suggested settings for side-window that opens on right
if there is space and the bottom otherwise."
  (interactive)
  (setq which-key-popup-type 'side-window
        which-key-side-window-location '(right bottom)
        which-key-show-prefix 'top))

;;;###autoload
(defun which-key-setup-side-window-bottom ()
  "Apply suggested settings for side-window that opens on
bottom."
  (interactive)
  (which-key--setup-echo-keystrokes)
  (setq which-key-popup-type 'side-window
        which-key-side-window-location 'bottom
        which-key-show-prefix 'echo))

;;;###autoload
(defun which-key-setup-minibuffer ()
  "Apply suggested settings for minibuffer.
Do not use this setup if you use the paging commands. Instead use
`which-key-setup-side-window-bottom', which is nearly identical
but more functional."
  (interactive)
  (which-key--setup-echo-keystrokes)
  (setq which-key-popup-type 'minibuffer
        which-key-show-prefix 'left))

;;; Helper functions to modify replacement lists.

;;;###autoload
(defun which-key-add-key-based-replacements
    (key-sequence replacement &rest more)
  "Replace the description of KEY-SEQUENCE with REPLACEMENT.
KEY-SEQUENCE is a string suitable for use in `kbd'. REPLACEMENT
may either be a string, as in

\(which-key-add-key-based-replacements \"C-x 1\" \"maximize\"\)

a cons of two strings as in

\(which-key-add-key-based-replacements \"C-x 8\"
                                        '(\"unicode\" . \"Unicode keys\")\)

or a function that takes a \(KEY . BINDING\) cons and returns a
replacement.

In the second case, the second string is used to provide a longer
name for the keys under a prefix.

MORE allows you to specifcy additional KEY REPLACEMENT pairs.  All
replacements are added to
`which-key-key-based-description-replacement-alist'."
  ;; TODO: Make interactive
  (while key-sequence
    ;; normalize key sequences before adding
    (let ((key-seq (key-description (kbd key-sequence)))
          (replace (or (and (functionp replacement) replacement)
                       (car-safe replacement)
                       replacement)))
      (push (cons (cons (concat "\\`" (regexp-quote key-seq) "\\'") nil)
                  (if (functionp replace) replace (cons nil replace)))
            which-key-replacement-alist)
      (when (and (not (functionp replacement)) (consp replacement))
        (push (cons key-seq (cdr-safe replacement))
              which-key--prefix-title-alist)))
    (setq key-sequence (pop more) replacement (pop more))))
(put 'which-key-add-key-based-replacements 'lisp-indent-function 'defun)

;;;###autoload
(defun which-key-add-major-mode-key-based-replacements
    (mode key-sequence replacement &rest more)
  "Functions like `which-key-add-key-based-replacements'.
The difference is that MODE specifies the `major-mode' that must
be active for KEY-SEQUENCE and REPLACEMENT (MORE contains
addition KEY-SEQUENCE REPLACEMENT pairs) to apply."
  ;; TODO: Make interactive
  (when (not (symbolp mode))
    (error "MODE should be a symbol corresponding to a value of major-mode"))
  (let ((mode-alist
         (or (cdr-safe (assq mode which-key-replacement-alist)) (list)))
        (title-mode-alist
         (or (cdr-safe (assq mode which-key--prefix-title-alist)) (list))))
    (while key-sequence
    ;; normalize key sequences before adding
      (let ((key-seq (key-description (kbd key-sequence)))
            (replace (or (and (functionp replacement) replacement)
                         (car-safe replacement)
                         replacement)))
        (push (cons (cons (concat "\\`" (regexp-quote key-seq) "\\'") nil)
                    (if (functionp replace) replace (cons nil replace)))
              mode-alist)
        (when (and (not (functionp replacement)) (consp replacement))
          (push (cons key-seq (cdr-safe replacement))
                title-mode-alist)))
      (setq key-sequence (pop more) replacement (pop more)))
    (if (assq mode which-key-replacement-alist)
        (setcdr (assq mode which-key-replacement-alist) mode-alist)
      (push (cons mode mode-alist) which-key-replacement-alist))
    (if (assq mode which-key--prefix-title-alist)
        (setcdr (assq mode which-key--prefix-title-alist) title-mode-alist)
      (push (cons mode title-mode-alist) which-key--prefix-title-alist))))
(put 'which-key-add-major-mode-key-based-replacements
     'lisp-indent-function 'defun)

(defalias 'which-key-add-prefix-title 'which-key-add-key-based-replacements)
(make-obsolete 'which-key-add-prefix-title
               'which-key-add-key-based-replacements
               "2016-10-05")

(defalias 'which-key-declare-prefixes 'which-key-add-key-based-replacements)
(make-obsolete 'which-key-declare-prefixes
               'which-key-add-key-based-replacements
               "2016-10-05")

(defalias 'which-key-declare-prefixes-for-mode
  'which-key-add-major-mode-key-based-replacements)
(make-obsolete 'which-key-declare-prefixes-for-mode
               'which-key-add-major-mode-key-based-replacements
               "2016-10-05")

(defun which-key-define-key-recursively (map key def &optional at-root)
  "Recursively bind KEY in MAP to DEF on every level of MAP except the first.
If AT-ROOT is non-nil the binding is also placed at the root of MAP."
  (when at-root (define-key map key def))
  (map-keymap
   (lambda (_ev df)
     (when (keymapp df)
       (which-key-define-key-recursively df key def t)))
   map))

(defun which-key--process-define-key-args (keymap key def)
  "When DEF takes the form (\"DESCRIPTION\". DEF), make sure
which-key uses \"DESCRIPTION\" for this binding. This function is
meant to be used as :before advice for `define-key'."
  (with-demoted-errors "Which-key extended define-key error: %s"
    (when (and (consp def)
               (stringp (car def))
               (symbolp (cdr def)))
      (define-key keymap (which-key--pseudo-key key) `(which-key ,def)))))

(when which-key-enable-extended-define-key
  (advice-add #'define-key :before #'which-key--process-define-key-args))

;;; Functions for computing window sizes

(defun which-key--text-width-to-total (text-width)
  "Convert window text-width to window total-width.
TEXT-WIDTH is the desired text width of the window.  The function
calculates what total width is required for a window in the
selected to have a text-width of TEXT-WIDTH columns.  The
calculation considers possible fringes and scroll bars.  This
function assumes that the desired window has the same character
width as the frame."
  (let ((char-width (frame-char-width)))
    (+ text-width
       (/ (frame-fringe-width) char-width)
       (/ (frame-scroll-bar-width) char-width)
       (if (which-key--char-enlarged-p) 1 0)
       ;; add padding to account for possible wide (unicode) characters
       3)))

(defun which-key--total-width-to-text (total-width)
  "Convert window total-width to window text-width.
TOTAL-WIDTH is the desired total width of the window.  The function calculates
what text width fits such a window.  The calculation considers possible fringes
and scroll bars.  This function assumes that the desired window has the same
character width as the frame."
  (let ((char-width (frame-char-width)))
    (- total-width
       (/ (frame-fringe-width) char-width)
       (/ (frame-scroll-bar-width) char-width)
       (if (which-key--char-enlarged-p) 1 0)
       ;; add padding to account for possible wide (unicode) characters
       3)))

(defun which-key--char-enlarged-p (&optional _frame)
  (> (frame-char-width)
     (/ (float (frame-pixel-width)) (window-total-width (frame-root-window)))))

(defun which-key--char-reduced-p (&optional _frame)
  (< (frame-char-width)
     (/ (float (frame-pixel-width)) (window-total-width (frame-root-window)))))

(defun which-key--char-exact-p (&optional _frame)
  (= (frame-char-width)
     (/ (float (frame-pixel-width)) (window-total-width (frame-root-window)))))

(defun which-key--width-or-percentage-to-width (width-or-percentage)
  "Return window total width.
If WIDTH-OR-PERCENTAGE is a whole number, return it unchanged.  Otherwise, it
should be a percentage (a number between 0 and 1) out of the frame's width.
More precisely, it should be a percentage out of the frame's root window's
total width."
  (if (wholenump width-or-percentage)
      width-or-percentage
    (round (* width-or-percentage (window-total-width (frame-root-window))))))

(defun which-key--height-or-percentage-to-height (height-or-percentage)
  "Return window total height.
If HEIGHT-OR-PERCENTAGE is a whole number, return it unchanged.  Otherwise, it
should be a percentage (a number between 0 and 1) out of the frame's height.
More precisely, it should be a percentage out of the frame's root window's
total height."
  (if (wholenump height-or-percentage)
      height-or-percentage
    (round (* height-or-percentage (window-total-height (frame-root-window))))))

(defun which-key--frame-size-changed-p ()
  "Non-nil if a change in frame size is detected."
  (let ((new-size (cons (frame-width) (frame-height))))
    (cond ((null which-key--previous-frame-size)
           (setq which-key--previous-frame-size new-size)
           nil)
          ((not (equal which-key--previous-frame-size new-size))
           (setq which-key--previous-frame-size new-size)))))

;;; Show/hide which-key buffer

(defun which-key--hide-popup ()
  "This function is called to hide the which-key buffer."
  (unless (member real-this-command which-key--paging-functions)
    (setq which-key--last-try-2-loc nil)
    (setq which-key--pages-obj nil)
    (setq which-key--automatic-display nil)
    (setq which-key--prior-show-keymap-args nil)
    (when (and which-key-idle-secondary-delay
               which-key--secondary-timer-active)
      (which-key--start-timer))
    (which-key--lighter-restore)
    (cl-case which-key-popup-type
      ;; Not necessary to hide minibuffer
      ;; (minibuffer (which-key--hide-buffer-minibuffer))
      (side-window (which-key--hide-buffer-side-window))
      (frame (which-key--hide-buffer-frame))
      (custom (funcall which-key-custom-hide-popup-function)))))

(defun which-key--hide-popup-ignore-command ()
  "Version of `which-key--hide-popup' without the check of
`real-this-command'."
  (cl-case which-key-popup-type
    (side-window (which-key--hide-buffer-side-window))
    (frame (which-key--hide-buffer-frame))
    (custom (funcall which-key-custom-hide-popup-function))))

(defun which-key--hide-popup-on-frame-size-change (&optional _)
  "Hide which-key popup if the frame is resized (to trigger a new
popup)."
  (when (which-key--frame-size-changed-p)
    (which-key--hide-popup)))

(defun which-key--hide-buffer-side-window ()
  "Hide which-key buffer when side-window popup is used."
  (when (buffer-live-p which-key--buffer)
    ;; in case which-key buffer was shown in an existing window, `quit-window'
    ;; will re-show the previous buffer, instead of closing the window
    (quit-windows-on which-key--buffer)))

(defun which-key--hide-buffer-frame ()
  "Hide which-key buffer when frame popup is used."
  (when (frame-live-p which-key--frame)
    (delete-frame which-key--frame)))

(defun which-key--popup-showing-p ()
  (window-live-p (get-buffer-window which-key--buffer)))

(defun which-key--show-popup (act-popup-dim)
  "Show the which-key buffer.
ACT-POPUP-DIM includes the dimensions, (height . width) of the
buffer text to be displayed in the popup.  Return nil if no window
is shown, or if there is no need to start the closing timer."
  (when (and (> (car act-popup-dim) 0) (> (cdr act-popup-dim) 0))
    (cl-case which-key-popup-type
      ;; Not called for minibuffer
      ;; (minibuffer (which-key--show-buffer-minibuffer act-popup-dim))
      (side-window (which-key--show-buffer-side-window act-popup-dim))
      (frame (which-key--show-buffer-frame act-popup-dim))
      (custom (funcall which-key-custom-show-popup-function act-popup-dim)))))

(defun which-key--fit-buffer-to-window-horizontally
    (&optional window &rest params)
  "Slightly modified version of `fit-buffer-to-window'.
Use &rest params because `fit-buffer-to-window' has a different
call signature in different emacs versions"
  (let ((fit-window-to-buffer-horizontally t))
    (apply #'fit-window-to-buffer window params)))

(defun which-key--show-buffer-side-window (act-popup-dim)
  "Show which-key buffer when popup type is side-window."
  (let* ((height (car act-popup-dim))
         (width (cdr act-popup-dim))
         (alist
          (if which-key-allow-imprecise-window-fit
              `((window-width .  ,(which-key--text-width-to-total width))
                (window-height . ,height)
                (side . ,which-key-side-window-location)
                (slot . ,which-key-side-window-slot))
            `((window-width . which-key--fit-buffer-to-window-horizontally)
              (window-height . (lambda (w) (fit-window-to-buffer w nil 1)))
              (side . ,which-key-side-window-location)
              (slot . ,which-key-side-window-slot)))))
    ;; Previously used `display-buffer-in-major-side-window' here, but
    ;; apparently that is meant to be an internal function. See emacs bug #24828
    ;; and advice given there.
    (cond
     ((eq which-key--multiple-locations t)
      ;; possibly want to switch sides in this case so we can't reuse the window
      (delete-windows-on which-key--buffer)
      (display-buffer-in-side-window which-key--buffer alist))
     ((get-buffer-window which-key--buffer)
      (display-buffer-reuse-window which-key--buffer alist))
     (t
      (display-buffer-in-side-window which-key--buffer alist)))))

(defun which-key--show-buffer-frame (act-popup-dim)
  "Show which-key buffer when popup type is frame."
  (let* (;(orig-window (selected-window))
         (frame-height (+ (car act-popup-dim)
                          (if (with-current-buffer which-key--buffer
                                mode-line-format)
                              1
                            0)))
         ;; without adding 2, frame sometimes isn't wide enough for the buffer.
         ;; this is probably because of the fringes. however, setting fringes
         ;; sizes to 0 (instead of adding 2) didn't always make the frame wide
         ;; enough. don't know why it is so.
         (frame-width (+ (cdr act-popup-dim) 2))
         (new-window (if (and (frame-live-p which-key--frame)
                              (eq which-key--buffer
                                  (window-buffer
                                   (frame-root-window which-key--frame))))
                         (which-key--show-buffer-reuse-frame
                          frame-height frame-width)
                       (which-key--show-buffer-new-frame
                        frame-height frame-width))))
    (when new-window
      ;; display successful
      (setq which-key--frame (window-frame new-window))
      new-window)))

(defun which-key--show-buffer-new-frame (frame-height frame-width)
  "Helper for `which-key--show-buffer-frame'."
  (let* ((frame-params `((height . ,frame-height)
                         (width . ,frame-width)
                         ;; tell the window manager to respect the given sizes
                         (user-size . t)
                         ;; which-key frame doesn't need a minibuffer
                         (minibuffer . nil)
                         (name . "which-key")
                         ;; no need for scroll bars in which-key frame
                         (vertical-scroll-bars . nil)
                         ;; (left-fringe . 0)
                         ;; (right-fringe . 0)
                         ;; (right-divider-width . 0)
                         ;; make sure frame is visible
                         (visibility . t)))
         (alist `((pop-up-frame-parameters . ,frame-params)))
         (orig-frame (selected-frame))
         (new-window (display-buffer-pop-up-frame which-key--buffer alist)))
    (when new-window
      ;; display successful
      (redirect-frame-focus (window-frame new-window) orig-frame)
      new-window)))

(defun which-key--show-buffer-reuse-frame (frame-height frame-width)
  "Helper for `which-key--show-buffer-frame'."
  (let ((window
         (display-buffer-reuse-window
          which-key--buffer `((reusable-frames . ,which-key--frame)))))
    (when window
      ;; display successful
      (set-frame-size (window-frame window) frame-width frame-height)
      window)))

;;; Max dimension of available window functions

(defun which-key--popup-max-dimensions ()
  "Dimesion functions should return the maximum possible (height
. width) of the intended popup. SELECTED-WINDOW-WIDTH is the
width of currently active window, not the which-key buffer
window."
  (cl-case which-key-popup-type
    (minibuffer (which-key--minibuffer-max-dimensions))
    (side-window (which-key--side-window-max-dimensions))
    (frame (which-key--frame-max-dimensions))
    (custom (funcall which-key-custom-popup-max-dimensions-function
                     (window-width)))))

(defun which-key--minibuffer-max-dimensions ()
  "Return max-dimensions of minibuffer (height . width).
Measured in lines and characters respectively."
  (cons
   ;; height
   (if (floatp max-mini-window-height)
       (floor (* (frame-text-lines)
                 max-mini-window-height))
     max-mini-window-height)
   ;; width
   (max 0 (- (frame-text-cols) which-key-unicode-correction))))

(defun which-key--side-window-max-dimensions ()
  "Return max-dimensions of the side-window popup (height .
width) in lines and characters respectively."
  (cons
   ;; height
   (if (member which-key-side-window-location '(left right))
       ;; 1 is a kludge to make sure there is no overlap
       (- (frame-height) (window-text-height (minibuffer-window)) 1)
     ;; (window-mode-line-height which-key--window))
     ;; FIXME: change to something like
     ;; (min which-*-height (calculate-max-height))
     (which-key--height-or-percentage-to-height
      which-key-side-window-max-height))
   ;; width
   (max 0
        (- (if (member which-key-side-window-location '(left right))
               (which-key--total-width-to-text
                (which-key--width-or-percentage-to-width
                 which-key-side-window-max-width))
             (which-key--total-width-to-text
              (which-key--width-or-percentage-to-width
               1.0)))
           which-key-unicode-correction))))

(defun which-key--frame-max-dimensions ()
  "Return max-dimensions of the frame popup (height .
width) in lines and characters respectively."
  (cons which-key-frame-max-height which-key-frame-max-width))

;;; Sorting functions

(defun which-key--string< (a b &optional alpha)
  (let* ((da (downcase a))
         (db (downcase b)))
    (cond ((string-equal da db)
           (if which-key-sort-uppercase-first
               (string-lessp a b)
             (not (string-lessp a b))))
          (alpha (string-lessp da db))
          (t (string-lessp a b)))))

(defun which-key--key-description< (a b &optional alpha)
  "Sorting function used for `which-key-key-order' and
`which-key-key-order-alpha'."
  (save-match-data
    (let* ((rngrgxp "^\\([^ ]+\\) \\.\\. [^ ]+")
           (a (if (string-match rngrgxp a) (match-string 1 a) a))
           (b (if (string-match rngrgxp b) (match-string 1 b) b))
           (aem? (string-equal a ""))
           (bem? (string-equal b ""))
           (a1? (= 1 (length a)))
           (b1? (= 1 (length b)))
           (srgxp "^\\(RET\\|SPC\\|TAB\\|DEL\\|LFD\\|ESC\\|NUL\\)")
           (asp? (string-match-p srgxp a))
           (bsp? (string-match-p srgxp b))
           (prrgxp "^\\(M\\|C\\|S\\|A\\|H\\|s\\)-")
           (apr? (string-match-p prrgxp a))
           (bpr? (string-match-p prrgxp b))
           (afn? (string-match-p "<f[0-9]+>" a))
           (bfn? (string-match-p "<f[0-9]+>" b)))
      (cond ((or aem? bem?) (and aem? (not bem?)))
            ((and asp? bsp?)
             (if (string-equal (substring a 0 3) (substring b 0 3))
                 (which-key--key-description<
                  (substring a 3) (substring b 3) alpha)
               (which-key--string< a b alpha)))
            ((or asp? bsp?) asp?)
            ((and a1? b1?) (which-key--string< a b alpha))
            ((or a1? b1?) a1?)
            ((and afn? bfn?)
             (< (string-to-number
                 (replace-regexp-in-string "<f\\([0-9]+\\)>" "\\1" a))
                (string-to-number
                 (replace-regexp-in-string "<f\\([0-9]+\\)>" "\\1" b))))
            ((or afn? bfn?) afn?)
            ((and apr? bpr?)
             (if (string-equal (substring a 0 2) (substring b 0 2))
                 (which-key--key-description<
                  (substring a 2) (substring b 2) alpha)
               (which-key--string< a b alpha)))
            ((or apr? bpr?) apr?)
            (t (which-key--string< a b alpha))))))

(defsubst which-key-key-order-alpha (acons bcons)
  "Order key descriptions A and B.
Order is lexicographic within a \"class\", where the classes and
the ordering of classes are listed below.

special (SPC,TAB,...) < single char < mod (C-,M-,...) < other.
Sorts single characters alphabetically with lowercase coming
before upper."
  (which-key--key-description< (car acons) (car bcons) t))

(defsubst which-key-key-order (acons bcons)
  "Order key descriptions A and B.
Order is lexicographic within a \"class\", where the classes and
the ordering of classes are listed below.

special (SPC,TAB,...) < single char < mod (C-,M-,...) < other."
  (which-key--key-description< (car acons) (car bcons)))

(defsubst which-key-description-order (acons bcons)
  "Order descriptions of A and B.
Uses `string-lessp' after applying lowercase."
  (string-lessp (downcase (cdr acons)) (downcase (cdr bcons))))

(defsubst which-key--group-p (description)
  (or (string-match-p "^\\(group:\\|Prefix\\)" description)
      (keymapp (intern description))))

(defun which-key-prefix-then-key-order (acons bcons)
  "Order first by whether A and/or B is a prefix with no prefix
coming before a prefix. Within these categories order using
`which-key-key-order'."
  (let ((apref? (which-key--group-p (cdr acons)))
        (bpref? (which-key--group-p (cdr bcons))))
    (if (not (eq apref? bpref?))
        (and (not apref?) bpref?)
      (which-key-key-order acons bcons))))

(defun which-key-prefix-then-key-order-reverse (acons bcons)
  "Order first by whether A and/or B is a prefix with prefix
coming before a prefix. Within these categories order using
`which-key-key-order'."
  (let ((apref? (which-key--group-p (cdr acons)))
        (bpref? (which-key--group-p (cdr bcons))))
    (if (not (eq apref? bpref?))
        (and apref? (not bpref?))
      (which-key-key-order acons bcons))))

(defun which-key-local-then-key-order (acons bcons)
  "Order first by whether A and/or B is a local binding with
local bindings coming first. Within these categories order using
`which-key-key-order'."
  (let ((aloc? (which-key--local-binding-p acons))
        (bloc? (which-key--local-binding-p bcons)))
    (if (not (eq aloc? bloc?))
        (and aloc? (not bloc?))
      (which-key-key-order acons bcons))))

;;; Functions for retrieving and formatting keys

(defsubst which-key--string-width (maybe-string)
  "If MAYBE-STRING is a string use `which-key--string-width' o/w return 0."
  (if (stringp maybe-string) (string-width maybe-string) 0))

(defsubst which-key--safe-lookup-key (keymap key)
  "Version of `lookup-key' that allows KEYMAP to be nil. KEY is not checked."
  (when (keymapp keymap) (lookup-key keymap key)))

(defsubst which-key--butlast-string (str)
  (mapconcat #'identity (butlast (split-string str)) " "))

(defun which-key--get-replacements (key-binding &optional use-major-mode)
  (let ((alist (or (and use-major-mode
                        (cdr-safe
                         (assq major-mode which-key-replacement-alist)))
                   which-key-replacement-alist))
        res case-fold-search)
    (catch 'res
      (dolist (replacement alist)
        ;; these are mode specific ones to ignore. The mode specific case is
        ;; handled in the selection of alist
        (unless (symbolp (car replacement))
          (let ((key-regexp (caar replacement))
                (binding-regexp (cdar replacement)))
            (when (and (or (null key-regexp)
                           (string-match-p key-regexp
                                           (car key-binding)))
                       (or (null binding-regexp)
                           (string-match-p binding-regexp
                                           (cdr key-binding))))
              (push replacement res)
              (when (not which-key-allow-multiple-replacements)
                (throw 'res res)))))))
    (nreverse res)))

(defun which-key--get-pseudo-binding (key-binding &optional prefix)
  (let* ((pseudo-binding
          (key-binding (which-key--pseudo-key (kbd (car key-binding)) prefix)))
         (pseudo-binding (when pseudo-binding (cadr pseudo-binding)))
         (pseudo-desc (when pseudo-binding (car pseudo-binding)))
         (pseudo-def (when pseudo-binding (cdr pseudo-binding)))
         (real-def (key-binding (kbd (car key-binding))))
         ;; treat keymaps as if they're nil bindings. This creates the
         ;; possibility that we rename the wrong binding but this seems
         ;; unlikely.
         (real-def (unless (keymapp real-def) real-def)))
    (when (and pseudo-binding
               (eq pseudo-def real-def))
      (cons (car key-binding) pseudo-desc))))

(defun which-key--maybe-replace (key-binding &optional prefix)
  "Use `which-key--replacement-alist' to maybe replace KEY-BINDING.
KEY-BINDING is a cons cell of the form \(KEY . BINDING\) each of
which are strings. KEY is of the form produced by `key-binding'."
  (let* ((pseudo-binding (which-key--get-pseudo-binding key-binding prefix)))
    (if pseudo-binding
        pseudo-binding
      (let* ((mode-res (which-key--get-replacements key-binding t))
             (all-repls (or mode-res
                            (which-key--get-replacements key-binding))))
        (dolist (repl all-repls key-binding)
          (setq key-binding
                (cond ((or (not (consp repl)) (null (cdr repl)))
                       key-binding)
                      ((functionp (cdr repl))
                       (funcall (cdr repl) key-binding))
                      ((consp (cdr repl))
                       (cons
                        (cond ((and (caar repl) (cadr repl))
                               (replace-regexp-in-string
                                (caar repl) (cadr repl) (car key-binding) t))
                              ((cadr repl) (cadr repl))
                              (t (car key-binding)))
                        (cond ((and (cdar repl) (cddr repl))
                               (replace-regexp-in-string
                                (cdar repl) (cddr repl) (cdr key-binding) t))
                              ((cddr repl) (cddr repl))
                              (t (cdr key-binding))))))))))))

(defsubst which-key--current-key-list (&optional key-str)
  (append (listify-key-sequence (which-key--current-prefix))
          (when key-str
            (listify-key-sequence (kbd key-str)))))

(defsubst which-key--current-key-string (&optional key-str)
  (key-description (which-key--current-key-list key-str)))

(defun which-key--local-binding-p (keydesc)
  (eq (which-key--safe-lookup-key
       (current-local-map) (kbd (which-key--current-key-string (car keydesc))))
      (intern (cdr keydesc))))

(defun which-key--map-binding-p (map keydesc)
  "Does MAP contain KEYDESC = (key . binding)?"
  (or
   (when (bound-and-true-p evil-state)
     (let ((lookup
            (which-key--safe-lookup-key
             map
             (kbd (which-key--current-key-string
                   (format "<%s-state> %s" evil-state (car keydesc)))))))
       (or (eq lookup (intern (cdr keydesc)))
           (and (keymapp lookup) (string= (cdr keydesc) "Prefix Command")))))
   (let ((lookup
          (which-key--safe-lookup-key
           map (kbd (which-key--current-key-string (car keydesc))))))
     (or (eq lookup (intern (cdr keydesc)))
         (and (keymapp lookup) (string= (cdr keydesc) "Prefix Command"))))))

(defun which-key--pseudo-key (key &optional prefix)
  "Replace the last key in the sequence KEY by a special symbol
in order for which-key to allow looking up a description for the key."
  (let* ((seq (listify-key-sequence key))
         (final (intern (format "which-key-%s" (key-description (last seq))))))
    (if prefix
        (vconcat prefix (list final))
      (vconcat (butlast seq) (list final)))))

(defun which-key--maybe-get-prefix-title (keys)
  "KEYS is a string produced by `key-description'.
A title is possibly returned using
`which-key--prefix-title-alist'.  An empty stiring is returned if
no title exists."
  (cond
   ((not (string-equal keys ""))
    (let* ((title-res
            (cdr-safe (assoc-string keys which-key--prefix-title-alist)))
           (repl-res
            (cdr-safe (which-key--maybe-replace (cons keys ""))))
           (binding (key-binding (kbd keys)))
           (alternate (when (and binding (symbolp binding))
                        (symbol-name binding))))
      (cond (title-res title-res)
            ((not (string-equal repl-res "")) repl-res)
            ((and (eq which-key-show-prefix 'echo) alternate)
             alternate)
            ((and (member which-key-show-prefix '(bottom top))
                  (eq which-key-side-window-location 'bottom)
                  echo-keystrokes)
             (if alternate alternate
               (concat "Following " keys)))
            (t ""))))
   (t "")))

(defun which-key--propertize (string &rest properties)
  "Version of `propertize' that checks type of STRING."
  (when (stringp string)
    (apply #'propertize string properties)))

(defun which-key--propertize-key (key)
  "Add a face to KEY.
If KEY contains any \"special keys\" defined in
`which-key-special-keys' then truncate and add the corresponding
`which-key-special-key-face'."
  (let ((key-w-face (which-key--propertize key 'face 'which-key-key-face))
        (regexp (concat "\\("
                        (mapconcat 'identity which-key-special-keys
                                   "\\|") "\\)"))
        case-fold-search)
    (save-match-data
      (if (and which-key-special-keys
               (string-match regexp key))
          (let ((beg (match-beginning 0)) (end (match-end 0)))
            (concat (substring key-w-face 0 beg)
                    (which-key--propertize (substring key-w-face beg (1+ beg))
                                'face 'which-key-special-key-face)
                    (substring key-w-face end
                               (which-key--string-width key-w-face))))
        key-w-face))))

(defsubst which-key--truncate-description (desc)
  "Truncate DESC description to `which-key-max-description-length'."
  (let* ((last-face (get-text-property (1- (length desc)) 'face desc))
         (dots (which-key--propertize ".." 'face last-face)))
    (if (and which-key-max-description-length
             (> (length desc) which-key-max-description-length))
        (concat (substring desc 0 which-key-max-description-length) dots)
      desc)))

(defun which-key--highlight-face (description)
  "Return the highlight face for DESCRIPTION if it has one."
  (let (face)
    (dolist (el which-key-highlighted-command-list)
      (unless face
        (cond ((consp el)
               (when (string-match-p (car el) description)
                 (setq face (cdr el))))
              ((stringp el)
               (when (string-match-p el description)
                 (setq face 'which-key-highlighted-command-face)))
              (t
               (message "which-key: warning: element %s of \
which-key-highlighted-command-list is not a string or a cons
cell" el)))))
    face))

(defun which-key--propertize-description
    (description group local hl-face &optional original-description)
  "Add face to DESCRIPTION where the face chosen depends on
whether the description represents a group or a command. Also
make some minor adjustments to the description string, like
removing a \"group:\" prefix.

ORIGINAL-DESCRIPTION is the description given by
`describe-buffer-bindings'."
  (when description
    (let* ((desc description)
           (desc (if (string-match-p "^group:" desc)
                     (substring desc 6) desc))
           (desc (if group (concat which-key-prefix-prefix desc) desc)))
      (make-text-button
       desc nil
       'face (cond (hl-face hl-face)
                   (group 'which-key-group-description-face)
                   (local 'which-key-local-map-description-face)
                   (t 'which-key-command-description-face))
       'help-echo (cond
                   ((and original-description
                         (fboundp (intern original-description))
                         (documentation (intern original-description))
                         ;; tooltip-mode doesn't exist in emacs-nox
                         (boundp 'tooltip-mode) tooltip-mode)
                    (documentation (intern original-description)))
                   ((and original-description
                         (fboundp (intern original-description))
                         (documentation (intern original-description))
                         (let* ((doc (documentation
                                      (intern original-description)))
                                (str (replace-regexp-in-string "\n" " " doc))
                                (max (floor (* (frame-width) 0.8))))
                           (if (> (length str) max)
                               (concat (substring str 0 max) "...")
                             str))))))
      desc)))

(defun which-key--extract-key (key-str)
  "Pull the last key (or key range) out of KEY-STR."
  (save-match-data
    (let ((key-range-regexp "\\`.*\\([^ \t]+ \\.\\. [^ \t]+\\)\\'"))
      (if (string-match key-range-regexp key-str)
          (match-string 1 key-str)
        (car (last (split-string key-str " ")))))))

(defun which-key--maybe-add-docstring (current original)
  "Maybe concat a docstring to CURRENT and return result.
Specifically, do this if ORIGINAL is a command with a docstring
and `which-key-show-docstrings' is non-nil. If
`which-key-show-docstrings' is the symbol docstring-only, just
return the docstring."
  (let* ((orig-sym (intern original))
         (doc (when (commandp orig-sym)
                (documentation orig-sym)))
         (docstring (when doc
                      (which-key--propertize (car (split-string doc "\n"))
                                             'face 'which-key-docstring-face))))
    (cond ((not (and which-key-show-docstrings docstring))
           current)
          ((eq which-key-show-docstrings 'docstring-only)
           docstring)
          (t
           (format "%s %s" current docstring)))))

(defun which-key--format-and-replace (unformatted &optional prefix preserve-full-key)
  "Take a list of (key . desc) cons cells in UNFORMATTED, add
faces and perform replacements according to the three replacement
alists. Returns a list (key separator description)."
  (let ((sep-w-face
         (which-key--propertize which-key-separator
                                'face 'which-key-separator-face))
        (local-map (current-local-map))
        new-list)
    (dolist (key-binding unformatted)
      (let* ((key (car key-binding))
             (orig-desc (cdr key-binding))
             (group (which-key--group-p orig-desc))
             ;; At top-level prefix is nil
             (keys (if prefix
                       (concat (key-description prefix) " " key)
                     key))
             (local (eq (which-key--safe-lookup-key local-map (kbd keys))
                        (intern orig-desc)))
             (hl-face (which-key--highlight-face orig-desc))
             (key-binding (which-key--maybe-replace (cons keys orig-desc) prefix))
             (final-desc (which-key--propertize-description
                          (cdr key-binding) group local hl-face orig-desc)))
        (when final-desc
          (setq final-desc
                (which-key--truncate-description
                 (which-key--maybe-add-docstring final-desc orig-desc))))
        (when (consp key-binding)
          (push
           (list (which-key--propertize-key
                  (if preserve-full-key
                      (car key-binding)
                    (which-key--extract-key (car key-binding))))
                 sep-w-face
                 final-desc)
           new-list))))
    (nreverse new-list)))

(defun which-key--get-keymap-bindings (keymap &optional all prefix)
  "Retrieve top-level bindings from KEYMAP.
If ALL is non-nil, get all bindings, not just the top-level
ones. PREFIX is for internal use and should not be used."
  (let (bindings)
    (map-keymap
     (lambda (ev def)
       (let* ((key (append prefix (list ev)))
              (key-desc (key-description key)))
         (cond ((or (string-match-p
                     which-key--ignore-non-evil-keys-regexp key-desc)
                    (eq ev 'menu-bar)))
               ;; extract evil keys corresponding to current state
               ((and (keymapp def)
                     (boundp 'evil-state)
                     (bound-and-true-p evil-local-mode)
                     (string-match-p (format "<%s-state>$" evil-state) key-desc))
                (setq bindings
                      ;; this function keeps the latter of the two duplicates
                      ;; which will be the evil binding
                      (cl-remove-duplicates
                       (append bindings
                               (which-key--get-keymap-bindings def all prefix))
                       :test (lambda (a b) (string= (car a) (car b))))))
               ((and (keymapp def)
                     (string-match-p which-key--evil-keys-regexp key-desc)))
               ((and (keymapp def)
                     (or all
                         ;; event 27 is escape, so this will pick up meta
                         ;; bindings and hopefully not too much more
                         (and (numberp ev) (= ev 27))))
                (setq bindings
                      (append bindings
                              (which-key--get-keymap-bindings def t key))))
               (t
                (when def
                  (cl-pushnew
                   (cons key-desc
                         (cond
                          ((keymapp def) "Prefix Command")
                          ((symbolp def) (copy-sequence (symbol-name def)))
                          ((eq 'lambda (car-safe def)) "lambda")
                          ((eq 'menu-item (car-safe def)) "menu-item")
                          ((stringp def) def)
                          (t "unknown")))
                   bindings :test (lambda (a b) (string= (car a) (car b)))))))))
     keymap)
    bindings))

(defun which-key--compute-binding (binding)
  "Replace BINDING with remapped binding if it exists.

Requires `which-key-compute-remaps' to be non-nil"
  (let (remap)
    (if (and which-key-compute-remaps
             (setq remap (command-remapping (intern binding))))
        (copy-sequence (symbol-name remap))
      binding)))

(defun which-key--get-current-bindings (&optional prefix)
  "Generate a list of current active bindings."
  (let ((key-str-qt (regexp-quote (key-description prefix)))
        (buffer (current-buffer))
        (ignore-bindings '("self-insert-command" "ignore"
                           "ignore-event" "company-ignore"))
        (ignore-sections-regexp
         (eval-when-compile
           (regexp-opt '("Key translations" "Function key map translations"
                         "Input decoding map translations")))))
    (with-temp-buffer
      (setq-local indent-tabs-mode t)
      (setq-local tab-width 8)
      (describe-buffer-bindings buffer prefix)
      (goto-char (point-min))
      (let ((header-p (not (= (char-after) ?\f)))
            bindings header)
        (while (not (eobp))
          (cond
           (header-p
            (setq header (buffer-substring-no-properties
                          (point)
                          (line-end-position)))
            (setq header-p nil)
            (forward-line 3))
           ((= (char-after) ?\f)
            (setq header-p t))
           ((looking-at "^[ \t]*$"))
           ((or (not (string-match-p ignore-sections-regexp header)) prefix)
            (let ((binding-start (save-excursion
                                   (and (re-search-forward "\t+" nil t)
                                        (match-end 0))))
                  key binding)
              (when binding-start
                (setq key (buffer-substring-no-properties
                           (point) binding-start))
                (setq binding (buffer-substring-no-properties
                               binding-start
                               (line-end-position)))
                (save-match-data
                  (cond
                   ((member binding ignore-bindings))
                   ((string-match-p which-key--ignore-keys-regexp key))
                   ((and prefix
                         (string-match (format "^%s[ \t]\\([^ \t]+\\)[ \t]+$"
                                               key-str-qt) key))
                    (unless (assoc-string (match-string 1 key) bindings)
                      (push (cons (match-string 1 key)
                                  (which-key--compute-binding binding))
                            bindings)))
                   ((and prefix
                         (string-match
                          (format
                           "^%s[ \t]\\([^ \t]+\\) \\.\\. %s[ \t]\\([^ \t]+\\)[ \t]+$"
                           key-str-qt key-str-qt) key))
                    (let ((stripped-key (concat (match-string 1 key)
                                                " \.\. "
                                                (match-string 2 key))))
                      (unless (assoc-string stripped-key bindings)
                        (push (cons stripped-key
                                    (which-key--compute-binding binding))
                              bindings))))
                   ((string-match
                     "^\\([^ \t]+\\|[^ \t]+ \\.\\. [^ \t]+\\)[ \t]+$" key)
                    (unless (assoc-string (match-string 1 key) bindings)
                      (push (cons (match-string 1 key)
                                  (which-key--compute-binding binding))
                            bindings)))))))))
          (forward-line))
        (nreverse bindings)))))

(defun which-key--get-bindings (&optional prefix keymap filter recursive)
  "Collect key bindings.
If KEYMAP is nil, collect from current buffer using the current
key sequence as a prefix. Otherwise, collect from KEYMAP. FILTER
is a function to use to filter the bindings. If RECURSIVE is
non-nil, then bindings are collected recursively for all prefixes."
  (let* ((unformatted
          (cond ((keymapp keymap)
                 (which-key--get-keymap-bindings keymap recursive))
                (keymap
                 (error "%s is not a keymap" keymap))
                (t
                 (which-key--get-current-bindings prefix)))))
    (when filter
      (setq unformatted (cl-remove-if-not filter unformatted)))
    (when which-key-sort-order
      (setq unformatted
            (sort unformatted which-key-sort-order)))
    (which-key--format-and-replace unformatted prefix recursive)))

;;; Functions for laying out which-key buffer pages

(defun which-key--normalize-columns (columns)
  "Pad COLUMNS to the same length using empty strings."
  (let ((max-len (cl-reduce (lambda (a x) (max a (length x))) columns
                            :initial-value 0)))
    (mapcar
     (lambda (c)
       (if (< (length c) max-len)
           (append c (make-list (- max-len (length c)) ""))
         c))
     columns)))

(defsubst which-key--join-columns (columns)
  "Transpose columns into rows, concat rows into lines and rows into page."
  (let* ((padded (which-key--normalize-columns (nreverse columns)))
         (rows (apply #'cl-mapcar #'list padded)))
    (mapconcat (lambda (row) (mapconcat #'identity row " ")) rows "\n")))

(defsubst which-key--max-len (keys index)
  "Internal function for finding the max length of the INDEX
element in each list element of KEYS."
  (cl-reduce
   (lambda (x y) (max x (which-key--string-width (nth index y))))
   keys :initial-value 0))

(defun which-key--pad-column (col-keys)
  "Take a column of (key separator description) COL-KEYS,
calculate the max width in the column and pad all cells out to
that width."
  (let* ((col-key-width  (+ which-key-add-column-padding
                            (which-key--max-len col-keys 0)))
         (col-sep-width  (which-key--max-len col-keys 1))
         (col-desc-width (which-key--max-len col-keys 2))
         (col-width      (+ 1 col-key-width col-sep-width col-desc-width)))
    (cons col-width
          (mapcar (lambda (k)
                    (format (concat "%" (int-to-string col-key-width)
                                    "s%s%-" (int-to-string col-desc-width) "s")
                            (nth 0 k) (nth 1 k) (nth 2 k)))
                  col-keys))))

(defun which-key--partition-list (n list)
  "Partition LIST into N-sized sublists."
  (let (res)
    (while list
      (setq res (cons (cl-subseq list 0 (min n (length list))) res)
            list (nthcdr n list)))
    (nreverse res)))

(defun which-key--list-to-pages (keys avl-lines avl-width)
  "Convert list of KEYS to columns based on dimensions AVL-LINES and AVL-WIDTH.
Returns a `which-key--pages' object that holds the page strings,
as well as metadata."
  (let ((cols-w-widths (mapcar #'which-key--pad-column
                               (which-key--partition-list avl-lines keys)))
        (page-width 0) (n-pages 0) (n-keys 0) (n-columns 0)
        page-cols pages page-widths keys/page col)
    (if (> (apply #'max (mapcar #'car cols-w-widths)) avl-width)
        ;; give up if no columns fit
        nil
      (while cols-w-widths
        ;; start new page
        (cl-incf n-pages)
        (setq col (pop cols-w-widths))
        (setq page-cols (list (cdr col)))
        (setq page-width (car col))
        (setq n-keys (length (cdr col)))
        (setq n-columns 1)
        ;; add additional columns as long as they fit
        (while (and cols-w-widths
                    (or (null which-key-max-display-columns)
                        (< n-columns which-key-max-display-columns))
                    (<= (+ (caar cols-w-widths) page-width) avl-width))
          (setq col (pop cols-w-widths))
          (push (cdr col) page-cols)
          (cl-incf page-width (car col))
          (cl-incf n-keys (length (cdr col)))
          (cl-incf n-columns))
        (push (which-key--join-columns page-cols) pages)
        (push n-keys keys/page)
        (push page-width page-widths))
      (make-which-key--pages
       :pages (nreverse pages)
       :height avl-lines
       :widths (nreverse page-widths)
       :keys/page (reverse keys/page)
       :page-nums (number-sequence 1 n-pages)
       :num-pages n-pages
       :total-keys (apply #'+ keys/page)))))

(defun which-key--create-pages-1
    (keys available-lines available-width &optional min-lines vertical)
  "Create page strings using `which-key--list-to-pages'.
Will try to find the best number of rows and columns using the
given dimensions and the length and widths of ITEMS. Use VERTICAL
if the ITEMS are laid out vertically and the number of columns
should be minimized."
  (let ((result (which-key--list-to-pages
                 keys available-lines available-width))
        (min-lines (or min-lines 0))
        found prev-result)
    (if (or (null result)
            vertical
            (> (which-key--pages-num-pages result) 1)
            (= 1 available-lines))
        result
      ;; simple search for a fitting page
      (while (and (> available-lines min-lines)
                  (not found))
        (setq available-lines (- available-lines 1)
              prev-result result
              result (which-key--list-to-pages
                      keys available-lines available-width)
              found (> (which-key--pages-num-pages result) 1)))
      (if found prev-result result))))

(defun which-key--create-pages (keys &optional prefix-keys prefix-title)
  "Create page strings using `which-key--list-to-pages'.
Will try to find the best number of rows and columns using the
given dimensions and the length and wdiths of KEYS. SEL-WIN-WIDTH
is the width of the live window."
  (let* ((max-dims (which-key--popup-max-dimensions))
         (max-lines (car max-dims))
         (max-width (cdr max-dims))
         (prefix-desc (key-description prefix-keys))
         (full-prefix (which-key--full-prefix prefix-desc))
         (prefix (when (eq which-key-show-prefix 'left)
                   (+ 2 (which-key--string-width full-prefix))))
         (prefix-top-bottom (member which-key-show-prefix '(bottom top)))
         (avl-lines (if prefix-top-bottom (- max-lines 1) max-lines))
         (min-lines (min avl-lines which-key-min-display-lines))
         (avl-width (if prefix (- max-width prefix) max-width))
         (vertical (and (eq which-key-popup-type 'side-window)
                        (member which-key-side-window-location '(left right))))
         result)
    (setq result
          (which-key--create-pages-1
           keys avl-lines avl-width min-lines vertical))
    (when (and result
               (> (which-key--pages-num-pages result) 0))
      (setf (which-key--pages-prefix result) prefix-keys)
      (setf (which-key--pages-prefix-title result)
            (or prefix-title
                (which-key--maybe-get-prefix-title
                 (key-description prefix-keys))))
      result)))

(defun which-key--lighter-status ()
  "Possibly show number of keys and total in the mode line."
  (when which-key-show-remaining-keys
    (let ((n-shown (car (which-key--pages-keys/page which-key--pages-obj)))
          (n-tot (which-key--pages-total-keys which-key--pages-obj)))
      (setcar (cdr (assq 'which-key-mode minor-mode-alist))
              (format " WK: %s/%s keys" n-shown n-tot)))))

(defun which-key--lighter-restore ()
  "Restore the lighter for which-key."
  (when which-key-show-remaining-keys
    (setcar (cdr (assq 'which-key-mode minor-mode-alist))
            which-key-lighter)))

(defun which-key--echo (text)
  "Echo TEXT to minibuffer without logging."
  (let (message-log-max)
    (message "%s" text)))

(defun which-key--next-page-hint (prefix-keys)
  "Return string for next page hint."
  (let* ((paging-key (concat prefix-keys " " which-key-paging-key))
         (paging-key-bound (eq 'which-key-C-h-dispatch
                               (key-binding (kbd paging-key))))
         (key (if paging-key-bound which-key-paging-key "C-h")))
    (when which-key-use-C-h-commands
      (which-key--propertize (format "[%s paging/help]" key)
                             'face 'which-key-note-face))))

(eval-and-compile
  (if (fboundp 'universal-argument--description)
      (defalias 'which-key--universal-argument--description
        'universal-argument--description)
    (defun which-key--universal-argument--description ()
      ;; Backport of the definition of universal-argument--description in
      ;; emacs25 on 2015-12-04
      (when prefix-arg
        (concat "C-u"
                (pcase prefix-arg
                  (`(-) " -")
                  (`(,(and (pred integerp) n))
                   (let ((str ""))
                     (while (and (> n 4) (= (mod n 4) 0))
                       (setq str (concat str " C-u"))
                       (setq n (/ n 4)))
                     (if (= n 4) str (format " %s" prefix-arg))))
                  (_ (format " %s" prefix-arg))))))))

(defun which-key--full-prefix (prefix-keys &optional -prefix-arg dont-prop-keys)
  "Return a description of the full key sequence up to now,
including prefix arguments."
  (let* ((left (eq which-key-show-prefix 'left))
         (prefix-arg (if -prefix-arg -prefix-arg prefix-arg))
         (str (concat
               (which-key--universal-argument--description)
               (when prefix-arg " ")
               prefix-keys))
         (dash (if (and (not (string= prefix-keys ""))
                        (null left)) "-" "")))
    (if (or (eq which-key-show-prefix 'echo) dont-prop-keys)
        (concat str dash)
      (concat (which-key--propertize-key str)
              (which-key--propertize dash 'face 'which-key-key-face)))))

(defun which-key--get-popup-map ()
  "Generate transient-map for use in the top level binding display."
  (unless which-key--automatic-display
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd which-key-paging-key) #'which-key-C-h-dispatch)
      (when which-key-use-C-h-commands
        ;; Show next page even when C-h is pressed
        (define-key map (kbd "C-h") #'which-key-C-h-dispatch))
      map)))

(defun which-key--process-page (pages-obj)
  "Add information to the basic list of key bindings, including
if applicable the current prefix, the name of the current prefix,
and a page count."
  (let* ((page (car (which-key--pages-pages pages-obj)))
         (height (which-key--pages-height pages-obj))
         (n-pages (which-key--pages-num-pages pages-obj))
         (page-n (car (which-key--pages-page-nums pages-obj)))
         (prefix-desc (key-description (which-key--pages-prefix pages-obj)))
         (prefix-title (which-key--pages-prefix-title pages-obj))
         (full-prefix (which-key--full-prefix prefix-desc))
         (nxt-pg-hint (which-key--next-page-hint prefix-desc))
         ;; not used in left case
         (status-line
          (concat (which-key--propertize prefix-title 'face 'which-key-note-face)
                  (when (< 1 n-pages)
                    (which-key--propertize (format " (%s of %s)" page-n n-pages)
                                           'face 'which-key-note-face)))))
    (pcase which-key-show-prefix
      (`left
       (let* ((page-cnt (which-key--propertize (format "%s/%s" page-n n-pages)
                                               'face 'which-key-separator-face))
              (first-col-width (+ 2 (max (which-key--string-width full-prefix)
                                         (which-key--string-width page-cnt))))
              (prefix (format (concat "%-" (int-to-string first-col-width) "s")
                              full-prefix))
              (page-cnt (if (> n-pages 1)
                            (format
                             (concat "%-" (int-to-string first-col-width) "s")
                             page-cnt)
                          (make-string first-col-width 32)))
              lines first-line new-end)
         (if (= 1 height)
             (cons (concat prefix page) nil)
           (setq lines (split-string page "\n")
                 first-line (concat prefix (car lines) "\n" page-cnt)
                 new-end (concat "\n" (make-string first-col-width 32)))
           (cons
            (concat first-line (mapconcat #'identity (cdr lines) new-end))
            nil))))
      (`top
       (cons
        (concat (when (or (= 0 echo-keystrokes)
                          (not (eq which-key-side-window-location 'bottom)))
                  (concat full-prefix " "))
                status-line " " nxt-pg-hint "\n" page)
        nil))
      (`bottom
       (cons
        (concat page "\n"
                (when (or (= 0 echo-keystrokes)
                          (not (eq which-key-side-window-location 'bottom)))
                  (concat full-prefix " "))
                status-line " " nxt-pg-hint)
        nil))
      (`echo
       (cons page
             (lambda ()
               (which-key--echo
                (concat full-prefix (when prefix-desc " ")
                        status-line (when status-line " ")
                        nxt-pg-hint)))))
      (`mode-line
       (cons page
             (lambda ()
               (with-current-buffer which-key--buffer
                 (setq-local mode-line-format
                             (concat " " full-prefix
                                     " " status-line
                                     " " nxt-pg-hint))))))
      (_ (cons page nil)))))

(defun which-key--show-page (&optional n)
  "Show current page. N changes the current page to the Nth page
relative to the current one."
  (which-key--init-buffer) ;; in case it was killed
  (let ((prefix-keys (which-key--current-key-string))
        golden-ratio-mode)
    (if (null which-key--pages-obj)
        (message "%s- which-key can't show keys: There is not \
enough space based on your settings and frame size." prefix-keys)
      (when n
        (setq which-key--pages-obj
              (which-key--pages-set-current-page which-key--pages-obj n)))
      (let ((page-echo (which-key--process-page which-key--pages-obj))
            (height (which-key--pages-height which-key--pages-obj))
            (width (car (which-key--pages-widths which-key--pages-obj))))
        (which-key--lighter-status)
        (if (eq which-key-popup-type 'minibuffer)
            (which-key--echo (car page-echo))
          (with-current-buffer which-key--buffer
            (erase-buffer)
            (insert (car page-echo))
            (goto-char (point-min)))
          (when (cdr page-echo) (funcall (cdr page-echo)))
          (which-key--show-popup (cons height width)))))
    ;; used for paging at top-level
    (if (fboundp 'set-transient-map)
        (set-transient-map (which-key--get-popup-map))
      (with-no-warnings
        (set-temporary-overlay-map (which-key--get-popup-map))))))

;;; Paging functions

;;;###autoload
(defun which-key-reload-key-sequence (&optional key-seq)
  "Simulate entering the key sequence KEY-SEQ.
KEY-SEQ should be a list of events as produced by
`listify-key-sequence'. If nil, KEY-SEQ defaults to
`which-key--current-key-list'. Any prefix arguments that were
used are reapplied to the new key sequence."
  (let* ((key-seq (or key-seq (which-key--current-key-list)))
         (next-event (mapcar (lambda (ev) (cons t ev)) key-seq)))
    (setq prefix-arg current-prefix-arg
          unread-command-events next-event)))

(defun which-key-turn-page (delta)
  "Show the next page of keys."
  (which-key-reload-key-sequence)
  (if which-key--last-try-2-loc
      (let ((which-key-side-window-location which-key--last-try-2-loc)
            (which-key--multiple-locations t))
        (which-key--show-page delta))
    (which-key--show-page delta))
  (which-key--start-paging-timer))

;;;###autoload
(defun which-key-show-standard-help (&optional _)
  "Call the command in `which-key--prefix-help-cmd-backup'.
Usually this is `describe-prefix-bindings'."
  (interactive)
  (let ((which-key-inhibit t))
    (which-key--hide-popup-ignore-command)
    (cond ((eq which-key--prefix-help-cmd-backup
               'describe-prefix-bindings)
           ;; This is essentially what `describe-prefix-bindings' does
           (describe-bindings
            (kbd (which-key--current-key-string))))
          ((functionp which-key--prefix-help-cmd-backup)
           (funcall which-key--prefix-help-cmd-backup)))))

;;;###autoload
(defun which-key-show-next-page-no-cycle ()
  "Show next page of keys unless on the last page, in which case
call `which-key-show-standard-help'."
  (interactive)
  (let ((which-key-inhibit t))
    (if (which-key--on-last-page)
        (which-key-show-standard-help)
      (which-key-turn-page 1))))

;;;###autoload
(defun which-key-show-previous-page-no-cycle ()
  "Show previous page of keys unless on the first page, in which
case do nothing."
  (interactive)
  (let ((which-key-inhibit t))
    (unless (which-key--on-first-page)
      (which-key-turn-page -1))))

;;;###autoload
(defun which-key-show-next-page-cycle (&optional _)
  "Show the next page of keys, cycling from end to beginning
after last page."
  (interactive)
  (let ((which-key-inhibit t))
    (which-key-turn-page 1)))

;;;###autoload
(defun which-key-show-previous-page-cycle (&optional _)
  "Show the previous page of keys, cycling from beginning to end
after first page."
  (interactive)
  (let ((which-key-inhibit t))
    (which-key-turn-page -1)))

;;;###autoload
(defun which-key-show-top-level (&optional _)
  "Show top-level bindings."
  (interactive)
  (which-key--create-buffer-and-show nil nil nil "Top-level bindings"))

;;;###autoload
(defun which-key-show-major-mode ()
  "Show top-level bindings in the map of the current major mode.

This function will also detect evil bindings made using
`evil-define-key' in this map. These bindings will depend on the
current evil state. "
  (interactive)
  (let ((map-sym (intern (format "%s-map" major-mode))))
    (if (and (boundp map-sym) (keymapp (symbol-value map-sym)))
        (which-key--create-buffer-and-show
         nil nil
         (apply-partially #'which-key--map-binding-p (symbol-value map-sym))
         "Major-mode bindings")
      (message "which-key: No map named %s" map-sym))))

;;;###autoload
(defun which-key-undo-key (&optional _)
  "Undo last keypress and force which-key update."
  (interactive)
  (let* ((key-lst (butlast (which-key--current-key-list)))
         (which-key-inhibit t))
    (cond (which-key--prior-show-keymap-args
           (if (keymapp (cdr (car-safe which-key--prior-show-keymap-args)))
               (let ((args (pop which-key--prior-show-keymap-args)))
                 (which-key--show-keymap (car args) (cdr args)))
             (which-key--hide-popup)))
          (key-lst
           (which-key-reload-key-sequence key-lst)
           (which-key--create-buffer-and-show (apply #'vector key-lst)))
          (t (setq which-key--automatic-display nil)
             (which-key-show-top-level)))))
(defalias 'which-key-undo 'which-key-undo-key)

(defun which-key-abort (&optional _)
  "Abort key sequence."
  (interactive)
  (let ((which-key-inhibit t))
    (which-key--hide-popup-ignore-command)
    (keyboard-quit)))

(defun which-key-digit-argument (key)
  "Version of `digit-argument' for use in `which-key-C-h-map'."
  (interactive)
  (let ((last-command-event (string-to-char key)))
    (digit-argument key))
  (let ((current-prefix-arg prefix-arg))
    (which-key-reload-key-sequence)))

(defun which-key-toggle-docstrings (&optional _)
  "Toggle the display of docstrings."
  (interactive)
  (unless (eq which-key-show-docstrings 'docstring-only)
    (setq which-key-show-docstrings (null which-key-show-docstrings)))
  (which-key-reload-key-sequence)
  (which-key--create-buffer-and-show (which-key--current-prefix)))

;;;###autoload
(defun which-key-C-h-dispatch ()
  "Dispatch C-h commands by looking up key in
`which-key-C-h-map'. This command is always accessible (from any
prefix) if `which-key-use-C-h-commands' is non nil."
  (interactive)
  (if (not (which-key--popup-showing-p))
      (which-key-show-standard-help)
    (let* ((prefix-keys (which-key--current-key-string))
           (full-prefix (which-key--full-prefix prefix-keys current-prefix-arg t))
           (prompt (concat (when (string-equal prefix-keys "")
                             (which-key--propertize
                              (concat " "
                                      (which-key--pages-prefix-title
                                       which-key--pages-obj))
                              'face 'which-key-note-face))
                           full-prefix
                           (which-key--propertize
                            (substitute-command-keys
                             (concat
                              " \\<which-key-C-h-map>"
                              " \\[which-key-show-next-page-cycle]"
                              which-key-separator "next-page,"
                              " \\[which-key-show-previous-page-cycle]"
                              which-key-separator "previous-page,"
                              " \\[which-key-undo-key]"
                              which-key-separator "undo-key,"
                              " \\[which-key-toggle-docstrings]"
                              which-key-separator "toggle-docstrings,"
                              " \\[which-key-show-standard-help]"
                              which-key-separator "help,"
                              " \\[which-key-abort]"
                              which-key-separator "abort"
                              " 1..9"
                              which-key-separator "digit-arg"))
                            'face 'which-key-note-face)))
           (key (string (read-key prompt)))
           (cmd (lookup-key which-key-C-h-map key))
           (which-key-inhibit t))
      (if cmd (funcall cmd key) (which-key-turn-page 0)))))

;;; Update

(defun which-key--any-match-p (regexps string)
  "Non-nil if any of REGEXPS match STRING."
  (catch 'match
    (dolist (regexp regexps)
      (when (string-match-p regexp string)
        (throw 'match t)))))

(defun which-key--try-2-side-windows
    (bindings prefix-keys prefix-title loc1 loc2 &rest _ignore)
  "Try to show BINDINGS (PAGE-N) in LOC1 first.

Only if no bindings fit fallback to LOC2."
  (let (pages1)
    (let ((which-key-side-window-location loc1)
          (which-key--multiple-locations t))
      (setq pages1 (which-key--create-pages
                    bindings prefix-keys prefix-title)))
    (if pages1
        (progn
          (setq which-key--pages-obj pages1)
          (let ((which-key-side-window-location loc1)
                (which-key--multiple-locations t))
            (which-key--show-page))
          loc1)
      (let ((which-key-side-window-location loc2)
            (which-key--multiple-locations t))
        (setq which-key--pages-obj
              (which-key--create-pages bindings prefix-keys prefix-title))
        (which-key--show-page)
        loc2))))

(defun which-key--read-keymap ()
  "Read keymap symbol from minibuffer."
  (intern
   (completing-read "Keymap: " obarray
                    (lambda (m)
                      (and (boundp m)
                           (keymapp (symbol-value m))
                           (not (equal (symbol-value m)
                                       (make-sparse-keymap)))))
                    t
                    (let ((sym (symbol-at-point)))
                      (and (boundp sym)
                           (keymapp (symbol-value sym))
                           (symbol-name sym)))
                    'which-key-keymap-history)))

;;;###autoload
(defun which-key-show-keymap (keymap)
  "Show the top-level bindings in KEYMAP using which-key. KEYMAP
is selected interactively from all available keymaps."
  (interactive (list (which-key--read-keymap)))
  (which-key--show-keymap (symbol-name keymap)
                          (symbol-value keymap)))

;;;###autoload
(defun which-key-show-full-keymap (keymap)
  "Show all bindings in KEYMAP using which-key. KEYMAP is
selected interactively from all available keymaps."
  (interactive (list (which-key--read-keymap)))
  (which-key--show-keymap (symbol-name keymap)
                          (symbol-value keymap)
                          nil t))

;;;###autoload
(defun which-key-show-minor-mode-keymap ()
  "Show the top-level bindings in KEYMAP using which-key. KEYMAP
is selected interactively by mode in `minor-mode-map-alist'."
  (interactive)
  (let ((mode-sym
         (intern
          (completing-read
           "Minor Mode: "
           (mapcar 'car
                   (cl-remove-if-not
                    (lambda (entry)
                      (and (symbol-value (car entry))
                           (not (equal (cdr entry) (make-sparse-keymap)))))
                    minor-mode-map-alist))
           nil t nil 'which-key-keymap-history))))
    (which-key--show-keymap (symbol-name mode-sym)
                            (cdr (assq mode-sym minor-mode-map-alist)))))

(defun which-key--show-keymap (keymap-name keymap &optional prior-args all)
  (when prior-args (push prior-args which-key--prior-show-keymap-args))
  (let ((bindings (which-key--get-bindings nil keymap nil all)))
    (if (= (length bindings) 0)
        (message "which-key: No bindings found in %s" keymap-name)
      (cond ((listp which-key-side-window-location)
             (setq which-key--last-try-2-loc
                   (apply #'which-key--try-2-side-windows
                          bindings nil keymap-name
                          which-key-side-window-location)))
            (t (setq which-key--pages-obj
                     (which-key--create-pages bindings nil keymap-name))
               (which-key--show-page)))
      (let* ((key (key-description (list (read-key))))
             (next-def (lookup-key keymap (kbd key))))
        (cond ((and which-key-use-C-h-commands (string= "C-h" key))
               (which-key-C-h-dispatch))
              ((keymapp next-def)
               (which-key--hide-popup-ignore-command)
               (which-key--show-keymap (concat keymap-name " " key) next-def
                                       (cons keymap-name keymap)))
              (t (which-key--hide-popup)))))))

(defun which-key--evil-operator-filter (binding)
  (let ((def (intern (cdr binding))))
    (and (functionp def)
         (not (evil-get-command-property def :suppress-operator)))))

(defun which-key--show-evil-operator-keymap ()
  (if which-key--inhibit-next-operator-popup
      (setq which-key--inhibit-next-operator-popup nil)
    (let ((keymap
           (make-composed-keymap (list evil-operator-shortcut-map
                                       evil-operator-state-map
                                       evil-motion-state-map))))
      (when (keymapp keymap)
        (let ((formatted-keys
               (which-key--get-bindings
                nil keymap #'which-key--evil-operator-filter)))
          (cond ((= (length formatted-keys) 0)
                 (message "which-key: Keymap empty"))
                ((listp which-key-side-window-location)
                 (setq which-key--last-try-2-loc
                       (apply #'which-key--try-2-side-windows
                              formatted-keys nil "evil operator/motion keys"
                              which-key-side-window-location)))
                (t (setq which-key--pages-obj
                         (which-key--create-pages
                          formatted-keys
                          nil "evil operator/motion keys"))
                   (which-key--show-page)))))
      (let* ((key (key-description (list (read-key)))))
        (when (member key '("f" "F" "t" "T" "`"))
          ;; these keys trigger commands that read the next char manually
          (setq which-key--inhibit-next-operator-popup t))
        (cond ((and which-key-use-C-h-commands (string= "C-h" key))
               (which-key-C-h-dispatch))
              ((string= key "ESC")
               (which-key--hide-popup)
               (keyboard-quit))
              (t
               (which-key--hide-popup)
               (setq unread-command-events (listify-key-sequence key))))))))

(defun which-key--create-buffer-and-show
    (&optional prefix-keys from-keymap filter prefix-title)
  "Fill `which-key--buffer' with key descriptions and reformat.
Finally, show the buffer."
  (let ((start-time (when which-key--debug (current-time)))
        (formatted-keys (which-key--get-bindings
                         prefix-keys from-keymap filter))
        (prefix-desc (key-description prefix-keys)))
    (cond ((= (length formatted-keys) 0)
           (message "%s-  which-key: There are no keys to show" prefix-desc))
          ((listp which-key-side-window-location)
           (setq which-key--last-try-2-loc
                 (apply #'which-key--try-2-side-windows
                        formatted-keys prefix-keys prefix-title
                        which-key-side-window-location)))
          (t (setq which-key--pages-obj
                   (which-key--create-pages
                    formatted-keys prefix-keys prefix-title))
             (which-key--show-page)))
    (when which-key--debug
      (message "On prefix \"%s\" which-key took %.0f ms." prefix-desc
               (* 1000 (float-time (time-since start-time)))))))

(defun which-key--this-command-keys ()
  "Version of `this-single-command-keys' corrected for key-chords and god-mode."
  (let ((this-command-keys (this-single-command-keys)))
    (when (and (equal this-command-keys [key-chord])
               (bound-and-true-p key-chord-mode))
      (setq this-command-keys
            (condition-case nil
                (let ((rkeys (recent-keys)))
                  (vector 'key-chord
                          ;; Take the two preceding the last one, because the
                          ;; read-event call in key-chord seems to add a
                          ;; spurious key press to this list. Note this is
                          ;; different from guide-key's method which didn't work
                          ;; for me.
                          (aref rkeys (- (length rkeys) 3))
                          (aref rkeys (- (length rkeys) 2))))
              (error (progn
                       (message "which-key error in key-chord handling")
                       [key-chord])))))
    (when (and which-key--god-mode-support-enabled
               (bound-and-true-p god-local-mode)
               (eq this-command 'god-mode-self-insert))
      (setq this-command-keys (when which-key--god-mode-key-string
                          (kbd which-key--god-mode-key-string))))
    this-command-keys))

(defun which-key--update ()
  "Function run by timer to possibly trigger
`which-key--create-buffer-and-show'."
  (let ((prefix-keys (which-key--this-command-keys))
        delay-time)
    (cond ((and (> (length prefix-keys) 0)
                (or (keymapp (key-binding prefix-keys))
                    ;; Some keymaps are stored here like iso-transl-ctl-x-8-map
                    (keymapp (which-key--safe-lookup-key
                              key-translation-map prefix-keys))
                    ;; just in case someone uses one of these
                    (keymapp (which-key--safe-lookup-key
                              function-key-map prefix-keys)))
                (not which-key-inhibit)
                (or (null which-key-allow-regexps)
                    (which-key--any-match-p
                     which-key-allow-regexps (key-description prefix-keys)))
                (or (null which-key-inhibit-regexps)
                    (not
                     (which-key--any-match-p
                      which-key-inhibit-regexps (key-description prefix-keys))))
                ;; Do not display the popup if a command is currently being
                ;; executed
                (or (and which-key-allow-evil-operators
                         (bound-and-true-p evil-this-operator))
                    (and which-key--god-mode-support-enabled
                         (bound-and-true-p god-local-mode)
                         (eq this-command 'god-mode-self-insert))
                    (null this-command)))
           (when (and (not (equal prefix-keys (which-key--current-prefix)))
                      (or (null which-key-delay-functions)
                          (null (setq delay-time
                                      (run-hook-with-args-until-success
                                       'which-key-delay-functions
                                       (key-description prefix-keys)
                                       (length prefix-keys))))
                          (sit-for delay-time)))
             (setq which-key--automatic-display t)
             (which-key--create-buffer-and-show prefix-keys)
             (when (and which-key-idle-secondary-delay
                        (not which-key--secondary-timer-active))
               (which-key--start-timer which-key-idle-secondary-delay t))))
          ((and which-key-show-transient-maps
                (keymapp overriding-terminal-local-map)
                ;; basic test for it being a hydra
                (not (eq (lookup-key overriding-terminal-local-map "\C-u")
                         'hydra--universal-argument)))
           (which-key--create-buffer-and-show
            nil overriding-terminal-local-map))
          ((and which-key-show-operator-state-maps
                (bound-and-true-p evil-state)
                (eq evil-state 'operator)
                (not (which-key--popup-showing-p)))
           (which-key--show-evil-operator-keymap))
          (which-key--automatic-display
           (which-key--hide-popup)))))

;;; Timers

(defun which-key--start-timer (&optional delay secondary)
  "Activate idle timer to trigger `which-key--update'."
  (which-key--stop-timer)
  (setq which-key--secondary-timer-active secondary)
  (setq which-key--timer
        (run-with-idle-timer
         (if delay
             delay
           which-key-idle-delay) t #'which-key--update)))

(defun which-key--stop-timer ()
  "Deactivate idle timer for `which-key--update'."
  (when which-key--timer (cancel-timer which-key--timer)))

(defun which-key--start-paging-timer ()
  "Activate timer to restart which-key after paging."
  (when which-key--paging-timer (cancel-timer which-key--paging-timer))
  (which-key--stop-timer)
  (setq which-key--paging-timer
        (run-with-idle-timer
         0.2 t (lambda ()
                 (when (or (not (member real-last-command
                                        which-key--paging-functions))
                           (and (< 0 (length (this-single-command-keys)))
                                (not (equal (which-key--current-prefix)
                                            (which-key--this-command-keys)))))
                   (cancel-timer which-key--paging-timer)
                   (which-key--start-timer))))))

(provide 'which-key)
;;; which-key.el ends here
