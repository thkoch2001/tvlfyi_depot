;; if in a project
  ;; if ansi-term is an existing process for the current project
    ;; ok
    ;; create an ansi-term buffer for the current project
  ;; if ansi-term is an open window
    ;; if number of open windows == 1, visit MRU source code buffer
    ;; if number of open windows  > 1, (delete-window ansi-term-window)
  ;; if ansi-term is not an open window
    ;; open ansi-term other-window
;; else
  ;; (message "You are not currently in a project.")
