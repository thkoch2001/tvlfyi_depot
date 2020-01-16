;;; terminator.el --- Experimenting with theming Terminator -*- lexical-binding: t -*-
;; Author: William Carroll <wpcarro@gmail.com>

;;; Commentary:
;; I think most of this module is me getting carried away with the idea of
;; theming Terminator.  Terminator themes are defined in a themes.json file.  As
;; far as I know, Terminator does not support specifying these themes by name on
;; the command line, which would greatly simplify things.  Terminator does
;; support passing a --profile flag, however, which can be used to specify the
;; themes.  The idea, albeit quite awkward and over-engineered, was to create
;; these profile files on the fly and pass them to terminator.  After around 45
;; minutes of tinkering with this, the idea is starting to disenchant me.
;;
;; Alternative solutions include:
;; 1. Further investigating what other options Terminator supports.
;; 2. Using a different terminal emulator.
;; 3. Just right clicking Terminator and changing the themes manually.

;;; Code:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dependencies
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'prelude)
(require 'alist)
(require 'string)
(require 'json)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Library
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl-defstruct terminator/theme
  foreground-color
  background-color
  cursor-color
  palette)

(defvar terminator/palettes
  '((solarized-light . "#002831:#d11c24:#738a05:#a57706:#2176c7:#c61c6f:#259286:#eae3cb:#001e27:#bd3613:#475b62:#536870:#708284:#5956ba:#819090:#fcf4dc"))
  "Mapping of theme names to the color palette that terminator expects.")

(defconst terminator/profile-template "[global_config]
  enabled_plugins = LaunchpadBugURLHandler, LaunchpadCodeURLHandler, APTURLHandler, TerminatorThemes
[keybindings]
[profiles]
  [[default]]
    background_color = \"%s\"
    cursor_shape = ibeam
    cursor_color = \"%s\"
    font = Input Mono Medium 12
    foreground_color = \"%s\"
    show_titlebar = False
    scrollbar_position = hidden
    palette = \"%s\"
    use_system_font = False
[layouts]
  [[default]]
    [[[child1]]]
      parent = window0
      type = Terminal
      profile = Molokai
    [[[window0]]]
      parent = \"\"
      type = Window
[plugins]"
  "Template string of a terminator profile file.")

(cl-defun terminator/render-profile (&key foreground-color
                                          background-color
                                          cursor-color
                                          palette)
  "Create a terminator profile with THEME as the palette."
  (string/format terminator/profile-template
                 background-color
                 cursor-color
                 foreground-color
                 palette))

(defun terminator/as-heredoc (x)
  "Return an EOF-terminator heredoc of X."
  (string/format "<<EOF\n%s\nEOF" x))

(prelude/start-process
 :name "termination"
 :command (string/format "zsh -c terminator --profile=%s"
                         (->> 'solarized-light
                              terminator/render-profile
                              terminator/as-heredoc)))
(string/format terminator/profile-template
               (alist/get 'solarized-light terminator/palettes))

(provide 'terminator)
;;; terminator.el ends here
