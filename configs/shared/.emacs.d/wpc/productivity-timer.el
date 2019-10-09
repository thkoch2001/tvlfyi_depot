;;; productivity-timer.el --- Commonly used intervals for setting alarms while working -*- lexical-binding: t -*-
;; Author: William Carroll <wpcarro@gmail.com>

;;; Commentary:
;; Select common timer intervals with dmenu and play an alarm sound when
;; finished.
;;
;; This is heavily inspired by iOS's timer feature.


;;; Code:

(defconst productivity-timer/intervals
  '(1 2 3 4 5 10 15 20 30 45 60 120)
  "Commonly used intervals for timer amounts.")

;; `sleep-for' doesn't seem to work.  Perhaps `sit-for' won't be any better.
;; How can I use dunst to alert?
;; `run-at-time' may be the most promising option

(provide 'productivity-timer)
;;; productivity-timer.el ends here
