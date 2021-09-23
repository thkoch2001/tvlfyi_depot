;; Helper functions for creating the other files.

(defun wiktionary-lookup-at-point (ask-lang)
  (interactive "P")
  (let ((language (if ask-lang (read-string "Language code? ") "ru")))
    (browse-url (concat "https://ru.wiktionary.org/wiki/"
                        (thing-at-point 'word)))))
