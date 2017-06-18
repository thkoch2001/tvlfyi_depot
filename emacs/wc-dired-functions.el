(defun wc/dired-up-directory ()
  "Moves up a directory in `dired' killing the previous `dired' buffer."
  (interactive)
  (find-alternate-file ".."))
