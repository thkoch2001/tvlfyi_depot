;;; todo.el --- Bespoke task management system -*- lexical-binding: t -*-
;; Author: William Carroll <wpcarro@gmail.com>

;;; Commentary:
;; Marriage of my personal task-management system, which I've been using for 18
;; months and is a mixture of handwritten notes, iOS notes, and org-mode files,
;; with Emacs's famous `org-mode'.
;;
;; For me, I'd like a live, reactive state management system.  I'd like
;; `org-mode' to be a nice way of rendering my TODOs, but I think the
;; relationship with `org-mode' ends there.
;;
;; Intended to supplement my org-mode workflow.
;;
;; Wish-list:
;; - Daily emails for standups
;; - Templates for commonly occurring tasks

;; Dependencies
(require 'dash)
(require 'f)
(require 'macros)

;;; Code:

;; TODO: Classify habits as 'daily, 'weekly, 'monthly, 'yearly, 'event-driven

;; TODO: Consider serving these values up to a React webpage in Chrome.

;; TODO: Classify meetings as either 'recurrent or 'ad-hoc.

;; TODO: Support sorting by `type'.

;; TODO: Support work-queue idea for "Tomorrow's todos."

;; TODO: Support macro to generate all possible predicates for todo types.

;; TODO: Support export to org-mode file

;; TODO: Support generic way to quickly render a list

(defcustom todo/install-kbds? t
  "When t, install the keybindings.")

;; TODO: Add documentation.
(cl-defstruct todo type label)

;; TODO: Consider keeping this in Dropbox.
;; TODO: Support whether or not the todo is done.
(defconst todo/org-file-path "~/Dropbox/org/today.org")

;; TODO: Support remaining function for each type.
;; TODO: Support completed function for each type.

(defun todo/completed? (x)
  "Return t is `X' is marked complete."
  (todo-complete x))

;; TODO: Prefer `new-{task,habit,meeting}'.

(defun todo/completed (xs)
  "Return the todo items in `XS' that are marked complete."
  (->> xs
       (-filter #'todo/completed?)))

(defun todo/remaining (xs)
  "Return the todo items in `XS' that are not marked complete."
  (->> xs
       (-reject #'todo/completed?)))

(defun todo/task (label)
  "Convenience function for creating a task todo with `LABEL'."
  (make-todo
   :type 'task
   :label label))

(defun todo/meeting (label)
  "Convenience function for creating a meeting todo with `LABEL'."
  (make-todo
   :type 'meeting
   :label label))

(defun todo/habit (label)
  "Convenience function for creating a habit todo with `LABEL'."
  (make-todo
   :type 'habit
   :label label))

(defun todo/task? (x)
  "Return t if `X' is a task."
  (equal 'task (todo-type x)))

(defun todo/habit? (x)
  "Return t if `X' is a habit."
  (equal 'habit (todo-type x)))

(defun todo/meeting? (x)
  "Return t if `X' is a meeting."
  (equal 'meeting (todo-type x)))

(defun todo/label (x)
  "Return the label of `X'."
  (todo-label x))

;; TODO: Support moving todos between todo/{today,tomorrow}.
;; TODO: Consider modelling todo/{today,tomorrow} as queues instead of lists so that I can
;; append cheaply.

;; TODO: Find an Elisp date library.

;; TODO: type-driven development of this habit tree.
;; TODO: Create this tree on a whiteboard first.
;; (defconst todo/habits
;;   '(:beginning-of-month
;;     '("Create habit template for current month"
;;       "Post mortem of previous month")
;;     :monday    '("Jiu Jitsu")
;;     :tuesday   '("Jiu Jitsu")
;;     :wednesday '("Jiu Jitsu")
;;     :thursday  '("Salsa class")
;;     :friday    '("Jiu Jitsu")
;;     :saturday  '("Borough market")
;;     :sunday    '("Shave")
;;     :weekday '(:arrive-at-work
;;                '("Breakfast"
;;                  "Coffee"
;;                  "Placeholder")
;;                :before-lunch
;;                '("Lock laptop"
;;                  "Placeholder")
;;                :home->work
;;                '("Michel Thomas Italian lessons"))
;;     :daily '(:morning
;;              '("Meditate"
;;                "Stretch")
;;              :)))

;; overlay weekday with specific weekdays (e.g. BJJ is only on M,T,W)

;; TODO: Extend the record type to support duration estimations for AFK, K
;; calculations.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Habits
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: Should I be writing this in ReasonML and Haskell?

(defconst todo/monthly-habit-challenge
  "InterviewCake.com"
  "The monthly habit challenge I do for fifteen minutes each day.")

(defconst todo/daily-habits
  (->> (list "Meditate"
             todo/monthly-habit-challenge)
       (-map #'todo/habit)))

(defconst todo/first-of-the-month-stack
  '("Create habit template for current month"
    "Reserve two dinners in London for dates"
    "Post mortem of previous month"
    "Create monthly financial budget in Google Sheets")
  "A stack of habits that I do at the beginning of each month.")

(defconst todo/adhoc-habits
  (->> (list/concat
        todo/first-of-the-month-stack)
       (-map #'todo/habit))
  "Habits that I have no classification for at the moment.")

;; TODO: Model this as a function.
(defconst todo/habits
  (list/concat todo/daily-habits
               todo/adhoc-habits)
  "My habits for today.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Meetings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: Define "meeting".

(defconst todo/daily-meetings
  (->> '("Standup"
         "Lunch")
       (-map #'todo/meeting))
  "Daily, recurrent meetings.")


(defconst todo/day-of-week-meetings
  '(:Monday    '("Lunch")
    :Tuesday   '("Lunch")
    :Wednesday '("Team Lunch")
    :Thursday  '("Lunch")
    :Friday    '("Lunch")
    :Satuday   '()
    :Sunday    '())
  "Meetings that occur depending on the current day of the week.")

(parse-time-string "today")

;; TODO: Support recurrent, non-daily meetings.

(defconst todo/adhoc-meetings
  (->> '("WSE Weekly Standup"
         "Team Lunch"
         "Karisa Explains It All")
       (-map #'todo/meeting))
  "Non-recurrent meetings.")

(defconst todo/meetings
  (list/concat todo/daily-meetings
               todo/adhoc-meetings)
  "My meetings for today.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tasks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst todo/tasks
  (->> '("GetEmailCase"
         "Async node workflow"
         "Support C-c in EXWM"
         "Post-its for bathroom mirror"
         "Visit AtomicHabit.com/scorecard"
         "Visit AtomicHabit.com/habitstacking"
         "Create GraphViz for Carpe Diem cirriculum"
         "Create CitC client for local browsing of CE codebase"
         "Respond to SRE emails")
       (-map #'todo/task)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Work queues (today, tomorrow, someday)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: Generate standup documents from DONE items in the state.

;; TODO: Learn how to create a gen-server style of live, reactive state.
;; TODO: This should probably be `defconst' and a reference to the live state.
(defconst todo/today
  (list/concat
   todo/habits
   todo/meetings
   todo/tasks))

(defconst todo/tomorrow
  '())

(defconst todo/someday
  '())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; View functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun todo/to-org (xs)
  "Map `XS' into a string with `org-mode' syntax."
  ;; TODO: Create function to DRY this code up.
  (let ((meetings (->> xs
                       (-filter #'todo/meeting?)
                       (-map (lambda (x)
                               (s-concat "** TODO " (todo/label x))))
                       (s-join "\n")))
        (tasks (->> xs
                    (-filter #'todo/task?)
                    (-map (lambda (x)
                            (s-concat "** TODO " (todo/label x))))
                    (s-join "\n")))
        (habits (->> xs
                     (-filter #'todo/habit?)
                     (-map (lambda (x)
                             (s-concat "** TODO " (todo/label x))))
                     (s-join "\n"))))
    (s-join "\n" (list
                  (s-concat "* Meetings\n" meetings)
                  (s-concat "* Tasks\n" tasks)
                  (s-concat "* Habits\n" habits)))))

(defun todo/export-to-org (xs)
  "Export `XS' to `todo/org-file-path'."
  (f-write-text (->> xs
                     todo/to-org)
                'utf-8
                todo/org-file-path))

(defun todo/orgify-today ()
  "Exports today's todos to an org file."
  (interactive)
  (todo/export-to-org todo/today)
  (alert (string/concat  "Exported today's TODOs to: " todo/org-file-path)))

(when todo/install-kbds?
  (evil-leader/set-key
    "to" #'todo/orgify-today))

(provide 'todo)
;;; todo.el ends here
