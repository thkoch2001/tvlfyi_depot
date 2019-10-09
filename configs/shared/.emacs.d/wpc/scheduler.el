;;; scheduler.el --- Sketches of scheduling -*- lexical-binding: t -*-
;; Author: William Carroll <wpcarro@gmail.com>

;;; Commentary:
;; Attempting to create a FSM for scheduling things in various ways:
;;
;; Scheduling policies:
;; - earliest due date: minimizes total lateness of all tasks in a pool.  Put
;;   the task with the latest due date last in the list and work backwards to
;;   solve the precedence constraint (i.e. dependency issue).
;; - shortest processing time: maximizes number of tasks completed.  Prioritize
;;   tasks in the order of how long they will take to complete from shortest to
;;   longest.  This breaks down when precedence constraints are introduced.
;;
;; Tasks should inherit prioritization.



;;; Code:

(provide 'scheduler)
;;; scheduler.el ends here
