;;; git.el --- My version control preferences -*- lexical-binding: t -*-
;; Author: William Carroll <wpcarro@gmail.com>

;;; Commentary:
;; Things related to git, magit, etc belong here

;;; Code:

(use-package git-timemachine)

(use-package magit)

(use-package magit-gh-pulls
  :ghook ('magit-mode-hook #'turn-on-magit-gh-pulls))

(provide 'wpc-git)
;;; git.el ends here
