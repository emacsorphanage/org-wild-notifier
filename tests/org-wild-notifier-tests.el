;;; org-wild-notifier-tests.el --- Tests for org-wild-notifier

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides notification functions for org-agenda.
;; Notification times can be customized either globally (for all org
;; entries) through `org-wild-notifier-alert-time` variable or on per
;; org entry basis using `WILD_NOTIFIER_NOTIFY_BEFORE` property, which
;; in turn is customizable via
;; `org-wild-notifier-alert-times-property` variable.

;;; Code:

(require 'ert)
(require 'org-wild-notifier)
(require 'dash)
(require 'subr-x)

(defun async-start (fn callback)
  ;; Mock package-initialize to be a no-op for testing (it's slow and unnecessary)
  (cl-letf (((symbol-function 'package-initialize) #'ignore))
    (->> (funcall fn)
         (funcall callback))))

(cl-defmacro org-wild-notifier-test
    (test-name desc
               &key
               (time "15:30")
               (fixture "planning.org")
               (overrides nil)
               expected-alerts)
  `(ert-deftest ,test-name ()
     ,desc
     (cl-letf* ((org-agenda-files (list (expand-file-name ,fixture "fixtures")))
                (org-todo-keywords (list "TODO" "IN PROGRESS" "DONE"))
                (time-string (format "January 4, 2018 %s" ,time))
                ((symbol-function 'current-time)
                 (lambda () (apply 'encode-time (parse-time-string time-string))))
                (org-agenda-start-day (time-to-days (current-time)))
                ((symbol-function 'org-today)
                 (lambda () (time-to-days (current-time))))
                (registered-alerts nil)
                ((symbol-function 'alert)
                 (lambda (msg &rest _)
                   (setf registered-alerts (push msg registered-alerts))))
                ;; Set consistent defaults for testing
                (org-wild-notifier-display-time-format-string "%H:%M")
                (org-wild-notifier-keyword-whitelist '("TODO"))
                (org-wild-notifier-keyword-blacklist nil)
                (org-wild-notifier-tags-whitelist nil)
                (org-wild-notifier-tags-blacklist nil)
                (org-wild-notifier-predicate-whitelist nil)
                (org-wild-notifier-predicate-blacklist '(org-wild-notifier-done-keywords-predicate))
                ,@overrides)
       (progn
         (org-wild-notifier-check)
         (should (equal '(,@expected-alerts) registered-alerts))))))

(org-wild-notifier-test alert-time
  "Tests that it notifies before standard notification time (10 minutes)"
  :time "15:50"
  :expected-alerts
  ("event with raw date at 16:00 at 16:00 (in 10 minutes)"
   "TODO event at 16:00 with NOTIFY_BEFORE property set to 31 at 16:00 (in 10 minutes)"
   "TODO event at 16:00 with notifications before 80, 60, 55, 43 and 5 at 16:00 (in \
10 minutes)"
   "TODO event scheduled on 16:00 with deadline at 17:00 at 16:00 (in 10 minutes)"))

(org-wild-notifier-test alert-time-overriden
  "Test that standard alert time can be customized"
  :time "14:50"
  :overrides ((org-wild-notifier-alert-time 70))
  :expected-alerts
  ("event with raw date at 16:00 at 16:00 (in 1 hour 10 minutes)"
   "TODO event at 16:00 with NOTIFY_BEFORE property set to 31 at 16:00 (in \
1 hour 10 minutes)"
   "TODO event at 16:00 with notifications before 80, 60, 55, 43 and 5 at 16:00 (in \
1 hour 10 minutes)"
   "TODO event scheduled on 16:00 with deadline at 17:00 at 16:00 (in \
1 hour 10 minutes)"))

(org-wild-notifier-test alert-time-overriden-with-list
  "Test that standard alert time can be customized & set to list"
  :time "14:50"
  :overrides ((org-wild-notifier-alert-time '(10 70)))
  :expected-alerts
  ("event with raw date at 16:00 at 16:00 (in 1 hour 10 minutes)"
   "TODO event at 16:00 with NOTIFY_BEFORE property set to 31 at 16:00 (in \
1 hour 10 minutes)"
   "TODO event at 16:00 with notifications before 80, 60, 55, 43 and 5 at 16:00 (in \
1 hour 10 minutes)"
   "TODO event scheduled on 16:00 with deadline at 17:00 at 16:00 (in \
1 hour 10 minutes)"
   "TODO event at 15:00 at 15:00 (in 10 minutes)"))

(org-wild-notifier-test notification-property
  "Tests that notifier takes in account the notification property"
  :time "15:17"
  :expected-alerts
  ("TODO event at 16:00 with notifications before \
80, 60, 55, 43 and 5 at 16:00 (in 43 minutes)"))

(org-wild-notifier-test notification-property-overriden
  "Tests that it is possible to customize the notification property"
  :time "15:29"
  :overrides ((org-wild-notifier-alert-times-property "NOTIFY_BEFORE"))
  :expected-alerts
  ("TODO event at 16:00 with NOTIFY_BEFORE property set to 31 at 16:00 (in 31 minutes)"))

(org-wild-notifier-test message-within-one-minute
  "Tests that message is correct"
  :time "14:59"
  :overrides ((org-wild-notifier-alert-time 1))
  :expected-alerts
  ("TODO event at 15:00 at 15:00 (in 1 minute)"))

(org-wild-notifier-test day-wide-events
  "Tests that user receives notifications on day-wide (w/o specified time) \
events"
  :time "14:59"
  :overrides ((org-wild-notifier-day-wide-alert-times '("14:59")))
  :expected-alerts
  ("TODO event scheduled on today is due or scheduled today"
   "TODO event scheduled on today's noon is due or scheduled today"))

(org-wild-notifier-test next-day-events
  "Tests that user receives notifications on next day events"
  :time "23:50"
  :expected-alerts
  ("TODO event scheduled on tomorrow at midnight at 00:00 (in 10 minutes)"))

(org-wild-notifier-test keyword-whitelist
  "Tests that whitelist option filters out events."
  :time "15:50"
  :expected-alerts
  ("event with raw date at 16:00 at 16:00 (in 10 minutes)"
   "TODO event at 16:00 with NOTIFY_BEFORE property set to 31 at 16:00 (in 10 minutes)"
   "TODO event at 16:00 with notifications before 80, 60, 55, 43 and 5 at 16:00 (in 10 \
minutes)"
   "TODO event scheduled on 16:00 with deadline at 17:00 at 16:00 (in 10 minutes)"))

(org-wild-notifier-test keyword-whitelist-disabled
  "Tests that whitelist option can be disabled"
  :time "15:50"
  :overrides ((org-wild-notifier-keyword-whitelist nil)
              (org-wild-notifier-predicate-blacklist nil))
  :expected-alerts
  ("event with raw date at 16:00 at 16:00 (in 10 minutes)"
   "TODO event at 16:00 with NOTIFY_BEFORE property set to 31 at 16:00 (in 10 minutes)"
   "TODO event at 16:00 with notifications before 80, 60, 55, 43 and 5 at 16:00 (in 10 minutes)"
   "TODO event scheduled on 16:00 with deadline at 17:00 at 16:00 (in 10 minutes)"
   "Tagged event at 16:00 at 16:00 (in 10 minutes)"
   "Plain event at 16:00 at 16:00 (in 10 minutes)"
   "IN PROGRESS event at 16:00 at 16:00 (in 10 minutes)"
   "DONE event at 16:00 at 16:00 (in 10 minutes)"))

(org-wild-notifier-test keyword-whitelist-with-two-items
  "Tests that whitelist option can contain more than one items"
  :time "15:50"
  :overrides ((org-wild-notifier-keyword-whitelist '("IN PROGRESS" "DONE"))
              (org-wild-notifier-predicate-blacklist nil))
  :expected-alerts
  ("IN PROGRESS event at 16:00 at 16:00 (in 10 minutes)"
   "DONE event at 16:00 at 16:00 (in 10 minutes)"))

(org-wild-notifier-test keyword-blacklist
  "Tests that blacklist option filters out events."
  :time "15:50"
  :overrides ((org-wild-notifier-keyword-whitelist '())
              (org-wild-notifier-keyword-blacklist '("TODO" "DONE")))
  :expected-alerts
  ("Tagged event at 16:00 at 16:00 (in 10 minutes)"
   "Plain event at 16:00 at 16:00 (in 10 minutes)"
   "IN PROGRESS event at 16:00 at 16:00 (in 10 minutes)"))

(org-wild-notifier-test tags-whitelist
  "Tests that whitelist option filters out events."
  :time "15:50"
  :overrides ((org-wild-notifier-tags-whitelist '("bar"))
              (org-wild-notifier-keyword-whitelist '()))
  :expected-alerts
  ("Tagged event at 16:00 at 16:00 (in 10 minutes)"))

(org-wild-notifier-test two-tags-whitelist
  "Tests that whitelist option filters out events."
  :time "15:50"
  :overrides ((org-wild-notifier-tags-whitelist '("bar" "baz"))
              (org-wild-notifier-keyword-whitelist '()))
  :expected-alerts
  ("event with raw date at 16:00 at 16:00 (in 10 minutes)"
   "Tagged event at 16:00 at 16:00 (in 10 minutes)"))

(org-wild-notifier-test mixed-whitelist
  "Tests that whitelist option filters out events."
  :time "15:50"
  :overrides ((org-wild-notifier-tags-whitelist '("bar" "baz"))
              (org-wild-notifier-keyword-whitelist '("IN PROGRESS")))
  :expected-alerts
  ("event with raw date at 16:00 at 16:00 (in 10 minutes)"
   "Tagged event at 16:00 at 16:00 (in 10 minutes)"
   "IN PROGRESS event at 16:00 at 16:00 (in 10 minutes)"))

(org-wild-notifier-test mixed-tags-whitelist-blacklist
  "Tests that blacklist option filters out events."
  :time "15:50"
  :overrides ((org-wild-notifier-keyword-whitelist '())
              (org-wild-notifier-tags-whitelist '("baz"))
              (org-wild-notifier-tags-blacklist '("foo")))
  :expected-alerts
  ("event with raw date at 16:00 at 16:00 (in 10 minutes)"))

(org-wild-notifier-test event-without-a-keyword
  "Tests that blacklist option filters out events."
  :time "19:25"
  :overrides ((org-wild-notifier-keyword-whitelist '()))
  :expected-alerts
  ("event without a keyword at 19:35 at 19:35 (in 10 minutes)"))

(org-wild-notifier-test non-existent-fixture
  "Tests that it doesn't hang if there's a non-existent agenda file."
  :fixture "bad.org"
  :overrides ((org-agenda-skip-unavailable-files t))
  :expected-alerts ())

;;; Tests for WILD_NOTIFIER_NOTIFY_AT property (absolute notification times)

(org-wild-notifier-test notify-at-single-time
  "Tests that NOTIFY_AT property triggers notification at absolute time"
  :time "14:30"
  :expected-alerts
  ("TODO event with all notification types (scheduled reminder)"
   "TODO event with absolute notification time (scheduled reminder)"))

(org-wild-notifier-test notify-at-multiple-times
  "Tests that NOTIFY_AT property supports multiple timestamps"
  :time "15:00"
  :expected-alerts
  ("TODO event at 16:00 with notifications before 80, 60, 55, 43 and 5 at 16:00 (in 60 minutes)"
   "TODO event with multiple absolute notification times (scheduled reminder)"))

(org-wild-notifier-test notify-at-multiple-times-second
  "Tests that second NOTIFY_AT timestamp also triggers"
  :time "15:30"
  :expected-alerts
  ("TODO event with multiple absolute notification times (scheduled reminder)"))

(org-wild-notifier-test notify-at-repeating
  "Tests that repeating NOTIFY_AT timestamps work"
  :time "16:00"
  :expected-alerts
  ("TODO event with repeating absolute notification (scheduled reminder)"))

(org-wild-notifier-test notify-at-custom-property
  "Tests that NOTIFY_AT property name can be customized"
  :time "14:30"
  :overrides ((org-wild-notifier-notify-at-property "CUSTOM_NOTIFY_AT"))
  :expected-alerts ())

(org-wild-notifier-test notify-at-with-default-notification
  "Tests that NOTIFY_AT event also fires default notification at scheduled time"
  :time "17:50"
  :expected-alerts
  ("TODO event with all notification types at 18:00 (in 10 minutes)"
   "TODO event with absolute notification time at 18:00 (in 10 minutes)"))

(org-wild-notifier-test notify-at-and-notify-before-combined
  "Tests that an event with both NOTIFY_AT and NOTIFY_BEFORE fires all notification types"
  :time "14:30"
  :expected-alerts
  ("TODO event with all notification types (scheduled reminder)"
   "TODO event with absolute notification time (scheduled reminder)"))

(org-wild-notifier-test notify-at-and-notify-before-combined-relative
  "Tests that NOTIFY_BEFORE fires for event with NOTIFY_AT"
  :time "17:15"
  :expected-alerts
  ("TODO event with all notification types at 18:00 (in 45 minutes)"))

(org-wild-notifier-test deadline-notification-separate-from-scheduled
  "Tests that deadline fires separately from scheduled time"
  :time "16:50"
  :expected-alerts
  ("TODO event scheduled on 16:00 with deadline at 17:00 at 17:00 (in 10 minutes)"))

;;; Tests for org-wild-notifier-entries (timestamp type filtering)

(org-wild-notifier-test entries-deadline-only
  "Tests that org-wild-notifier-entries can filter to DEADLINE only"
  :time "20:50"
  :overrides ((org-wild-notifier-entries '("DEADLINE")))
  :expected-alerts
  ("TODO event with deadline only at 21:00 at 21:00 (in 10 minutes)"))

(org-wild-notifier-test entries-property-override-deadline
  "Tests that WILD_NOTIFIER_ENTRIES property overrides to DEADLINE only"
  :time "21:00"
  :expected-alerts
  ("TODO event with scheduled and deadline at 21:10 at 21:10 (in 10 minutes)"))

(org-wild-notifier-test entries-property-override-scheduled
  "Tests that WILD_NOTIFIER_ENTRIES property overrides to SCHEDULED only"
  :time "21:10"
  :expected-alerts
  ("TODO event with entries override to SCHEDULED at 21:20 at 21:20 (in 10 minutes)"))

(org-wild-notifier-test entries-property-inheritance
  "Tests that WILD_NOTIFIER_ENTRIES property is inherited from parent"
  :time "21:20"
  :overrides ((org-wild-notifier-tags-whitelist '("entries")))
  :expected-alerts
  ("TODO child event inherits entries override at 21:30 at 21:30 (in 10 minutes)"))
