;;; Code
(defun wc/conditionally-activate-tabbar ()
  "Only activate tabbar-mode if the new major-mode is whitelisted."
  (setq tabbar-whitelist '(prog-mode))
  (if (memq major-mode tabbar-whitelist)
      (tabbar-local-mode 1)
    (tabbar-local-mode nil)))
