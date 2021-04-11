(require 'org)

(setq org-html-postamble nil)

(defadvice org-export-grab-title-from-buffer
    (around org-export-grab-title-from-buffer-disable activate))
