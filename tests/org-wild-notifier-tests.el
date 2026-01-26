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

;;; Unit tests for internal functions

;;; Tests for get-notifications-for-event

;;; Tests for parse-notify-at-timestamp with repeating intervals

(ert-deftest parse-notify-at-timestamp-non-repeating ()
  "Tests that non-repeating timestamps are parsed literally."
  (cl-letf* ((time-string "January 4, 2018 15:00")
             ((symbol-function 'current-time)
              (lambda () (apply 'encode-time (parse-time-string time-string)))))
    (let ((result (org-wild-notifier--parse-notify-at-timestamp "<2018-01-04 Thu 14:30>")))
      (should (equal (format-time-string "%Y-%m-%d %H:%M" result) "2018-01-04 14:30")))))

(ert-deftest parse-notify-at-timestamp-daily-repeater ()
  "Tests that daily repeating timestamps advance correctly."
  (cl-letf* ((time-string "January 8, 2018 10:00")
             ((symbol-function 'current-time)
              (lambda () (apply 'encode-time (parse-time-string time-string)))))
    ;; Base is Jan 4, current is Jan 8, should advance to Jan 8 or later
    (let ((result (org-wild-notifier--parse-notify-at-timestamp "<2018-01-04 Thu 16:00 +1d>")))
      (should (not (time-less-p result (current-time)))))))

(ert-deftest parse-notify-at-timestamp-weekly-repeater ()
  "Tests that weekly repeating timestamps advance correctly."
  (cl-letf* ((time-string "January 18, 2018 10:00")
             ((symbol-function 'current-time)
              (lambda () (apply 'encode-time (parse-time-string time-string)))))
    ;; Base is Jan 4, current is Jan 18, should advance by weeks
    (let ((result (org-wild-notifier--parse-notify-at-timestamp "<2018-01-04 Thu 09:00 +1w>")))
      (should (not (time-less-p result (current-time)))))))

(ert-deftest parse-notify-at-timestamp-monthly-repeater ()
  "Tests that monthly repeating timestamps advance correctly."
  (cl-letf* ((time-string "March 5, 2018 10:00")
             ((symbol-function 'current-time)
              (lambda () (apply 'encode-time (parse-time-string time-string)))))
    ;; Base is Jan 4, current is Mar 5, should advance to Mar 4 or Apr 4
    (let ((result (org-wild-notifier--parse-notify-at-timestamp "<2018-01-04 Thu 09:00 +1m>")))
      (should (not (time-less-p result (current-time)))))))

(ert-deftest parse-notify-at-timestamp-yearly-repeater ()
  "Tests that yearly repeating timestamps advance correctly."
  (cl-letf* ((time-string "January 5, 2020 10:00")
             ((symbol-function 'current-time)
              (lambda () (apply 'encode-time (parse-time-string time-string)))))
    ;; Base is Jan 4 2018, current is Jan 5 2020, should advance to 2020 or 2021
    (let ((result (org-wild-notifier--parse-notify-at-timestamp "<2018-01-04 Thu 09:00 +1y>")))
      (should (not (time-less-p result (current-time)))))))

(ert-deftest parse-notify-at-timestamp-future-base ()
  "Tests that future non-repeating timestamps are returned as-is."
  (cl-letf* ((time-string "January 4, 2018 10:00")
             ((symbol-function 'current-time)
              (lambda () (apply 'encode-time (parse-time-string time-string)))))
    ;; Base is in the future (Jan 4 15:00 vs current Jan 4 10:00)
    (let ((result (org-wild-notifier--parse-notify-at-timestamp "<2018-01-04 Thu 15:00>")))
      (should (equal (format-time-string "%Y-%m-%d %H:%M" result) "2018-01-04 15:00")))))

;;; Tests for get-notifications-for-event edge cases

(ert-deftest notifications-for-event-empty-times ()
  "Tests behavior with an event that has empty times list."
  (let ((event '((times . (nil))
                 (title . "Empty event")
                 (intervals . (10))
                 (notify-at . nil))))
    ;; Should not crash, should return empty
    (should (equal (org-wild-notifier--get-notifications-for-event event) nil))))

(ert-deftest notifications-for-event-empty-intervals ()
  "Tests behavior with an event that has empty intervals list."
  (cl-letf* ((time-string "January 4, 2018 15:00")
             ((symbol-function 'current-time)
              (lambda () (apply 'encode-time (parse-time-string time-string)))))
    (let* ((event-time (time-add (current-time) 600))
           (event `((times . (((:timestamp-type scheduled
                                 :timestamp-string "<2018-01-04 Thu 15:10>"
                                 :time ,event-time))))
                    (title . "No intervals event")
                    (intervals . nil)
                    (notify-at . nil))))
      ;; Should not crash, should return empty for relative notifications
      (should (equal (org-wild-notifier--get-notifications-for-event event) nil)))))

(ert-deftest notifications-for-event-day-wide-filtered ()
  "Tests that day-wide events (no time component) are filtered out."
  (cl-letf* ((time-string "January 4, 2018 15:00")
             ((symbol-function 'current-time)
              (lambda () (apply 'encode-time (parse-time-string time-string)))))
    (let* ((event-time (time-add (current-time) 600))
           ;; Day-wide timestamp has no time component
           (event `((times . (((:timestamp-type scheduled
                                 :timestamp-string "<2018-01-04 Thu>"
                                 :time ,event-time))))
                    (title . "Day wide event")
                    (intervals . (10))
                    (notify-at . nil))))
      ;; Day-wide events should be filtered by org-wild-notifier--has-timestamp
      (should (equal (org-wild-notifier--get-notifications-for-event event) nil)))))

(ert-deftest notifications-for-event-zero-interval ()
  "Tests notification with zero interval (at exact event time)."
  (cl-letf* ((time-string "January 4, 2018 15:00")
             ((symbol-function 'current-time)
              (lambda () (apply 'encode-time (parse-time-string time-string)))))
    (let* ((event-time (time-add (current-time) 300))  ; 5 minutes from now
           (event `((times . (((:timestamp-type scheduled
                                 :timestamp-string "<2018-01-04 Thu 15:05>"
                                 :time ,event-time))))
                    (title . "Zero interval event")
                    (intervals . (0))
                    (notify-at . nil))))
      ;; Zero interval means notify at exact event time
      (let ((result (org-wild-notifier--get-notifications-for-event event)))
        (should (= (length result) 1))
        (should (equal (plist-get (car result) :minutes-before) 0))))))

(ert-deftest notifications-for-event-basic ()
  "Tests basic notification generation."
  (cl-letf* ((time-string "January 4, 2018 15:00")
             ((symbol-function 'current-time)
              (lambda () (apply 'encode-time (parse-time-string time-string)))))
    (let* ((event-time (time-add (current-time) 600))  ; 10 min from now
           (event `((times . (((:timestamp-type scheduled
                                 :timestamp-string "<2018-01-04 Thu 15:10>"
                                 :time ,event-time))))
                    (title . "Test event")
                    (intervals . (10 5))  ; Two intervals
                    (notify-at . nil))))
      (let ((result (org-wild-notifier--get-notifications-for-event event)))
        ;; Should have 2 notifications (one per interval)
        (should (= (length result) 2))
        ;; Both should be relative type
        (should (--all? (eq (plist-get it :type) 'relative) result))
        ;; Check intervals are correct
        (should (member 10 (--map (plist-get it :minutes-before) result)))
        (should (member 5 (--map (plist-get it :minutes-before) result)))))))

(ert-deftest notifications-for-event-with-absolute ()
  "Tests notification generation with absolute notify-at times."
  (cl-letf* ((time-string "January 4, 2018 15:00")
             ((symbol-function 'current-time)
              (lambda () (apply 'encode-time (parse-time-string time-string)))))
    (let* ((event-time (time-add (current-time) 600))
           (notify-time (time-add (current-time) 300))
           (event `((times . (((:timestamp-type scheduled
                                 :timestamp-string "<2018-01-04 Thu 15:10>"
                                 :time ,event-time))))
                    (title . "Test event")
                    (intervals . (10))
                    (notify-at . (,notify-time)))))
      (let ((result (org-wild-notifier--get-notifications-for-event event)))
        ;; Should have 2 notifications: one relative, one absolute
        (should (= (length result) 2))
        (should (--any? (eq (plist-get it :type) 'relative) result))
        (should (--any? (eq (plist-get it :type) 'absolute) result))))))

;;; Tests for extract-notify-at-times edge cases

(ert-deftest extract-notify-at-times-nil-property ()
  "Tests that nil property returns nil."
  (with-temp-buffer
    (org-mode)
    (insert "* Test heading\n")
    (goto-char (point-min))
    (let ((marker (point-marker)))
      (should (equal (org-wild-notifier--extract-notify-at-times marker) nil)))))

(ert-deftest extract-notify-at-times-empty-string ()
  "Tests behavior with empty string property value."
  (with-temp-buffer
    (org-mode)
    (insert "* Test heading\n")
    (insert "  :PROPERTIES:\n")
    (insert "  :WILD_NOTIFIER_NOTIFY_AT: \n")
    (insert "  :END:\n")
    (goto-char (point-min))
    (let ((marker (point-marker)))
      ;; Empty string should return nil or empty list
      (should (equal (org-wild-notifier--extract-notify-at-times marker) nil)))))

;;; Tests for get-entries-for-marker edge cases

(ert-deftest get-entries-for-marker-no-property ()
  "Tests fallback to default when no property is set."
  (with-temp-buffer
    (org-mode)
    (insert "* Test heading\n")
    (goto-char (point-min))
    (let ((marker (point-marker))
          (org-wild-notifier-entries '("DEADLINE" "SCHEDULED")))
      (should (equal (org-wild-notifier--get-entries-for-marker marker)
                     '("DEADLINE" "SCHEDULED"))))))

(ert-deftest get-entries-for-marker-single-value ()
  "Tests property with single value."
  (with-temp-buffer
    (org-mode)
    (insert "* Test heading\n")
    (insert "  :PROPERTIES:\n")
    (insert "  :WILD_NOTIFIER_ENTRIES: DEADLINE\n")
    (insert "  :END:\n")
    (goto-char (point-min))
    (let ((marker (point-marker)))
      (should (equal (org-wild-notifier--get-entries-for-marker marker)
                     '("DEADLINE"))))))

(ert-deftest get-entries-for-marker-multiple-values ()
  "Tests property with multiple space-separated values."
  (with-temp-buffer
    (org-mode)
    (insert "* Test heading\n")
    (insert "  :PROPERTIES:\n")
    (insert "  :WILD_NOTIFIER_ENTRIES: DEADLINE SCHEDULED TIMESTAMP\n")
    (insert "  :END:\n")
    (goto-char (point-min))
    (let ((marker (point-marker)))
      (should (equal (org-wild-notifier--get-entries-for-marker marker)
                     '("DEADLINE" "SCHEDULED" "TIMESTAMP"))))))

(ert-deftest get-entries-for-marker-extra-whitespace ()
  "Tests property with extra whitespace between values."
  (with-temp-buffer
    (org-mode)
    (insert "* Test heading\n")
    (insert "  :PROPERTIES:\n")
    (insert "  :WILD_NOTIFIER_ENTRIES:   DEADLINE   SCHEDULED  \n")
    (insert "  :END:\n")
    (goto-char (point-min))
    (let ((marker (point-marker)))
      ;; split-string should handle extra whitespace, but let's verify
      (let ((result (org-wild-notifier--get-entries-for-marker marker)))
        ;; Should not include empty strings from extra whitespace
        (should (--all? (not (string-empty-p it)) result))))))

;;; Tests comparing old vs new notification logic consistency

(ert-deftest notifications-consistency-basic ()
  "Tests that old and new notification logic produce consistent results."
  (cl-letf* ((time-string "January 4, 2018 15:50")
             ((symbol-function 'current-time)
              (lambda () (apply 'encode-time (parse-time-string time-string)))))
    (let* ((now (current-time))
           (event-time (time-add now 600))  ; 10 minutes from now
           (event `((times . (((:timestamp-type scheduled
                                 :timestamp-string "<2018-01-04 Thu 16:00>"
                                 :time ,event-time))))
                    (title . "Test event")
                    (intervals . (10))
                    (notify-at . nil))))
      ;; Old logic should find this notification (at current time)
      (let ((old-result (org-wild-notifier--notifications event)))
        (should (= (length old-result) 1)))
      ;; New logic should also produce a notification for this interval
      (let ((new-result (org-wild-notifier--get-notifications-for-event event)))
        (should (= (length new-result) 1))))))

(ert-deftest notifications-old-logic-point-in-time ()
  "Verifies old logic only matches notifications at exact current minute."
  (cl-letf* ((time-string "January 4, 2018 15:50")
             ((symbol-function 'current-time)
              (lambda () (apply 'encode-time (parse-time-string time-string)))))
    (let* ((now (current-time))
           (event-time (time-add now 660))  ; 11 minutes from now - NOT 10
           (event `((times . (((:timestamp-type scheduled
                                 :timestamp-string "<2018-01-04 Thu 16:01>"
                                 :time ,event-time))))
                    (title . "Test event")
                    (intervals . (10))
                    (notify-at . nil))))
      ;; Old logic should NOT find this (11 min != 10 min interval)
      (let ((old-result (org-wild-notifier--notifications event)))
        (should (= (length old-result) 0))))))

(ert-deftest notifications-new-logic-returns-all ()
  "Verifies new logic returns all notifications regardless of current time."
  (cl-letf* ((time-string "January 4, 2018 15:50")
             ((symbol-function 'current-time)
              (lambda () (apply 'encode-time (parse-time-string time-string)))))
    (let* ((now (current-time))
           (event-time (time-add now 900))  ; 15 minutes from now
           (event `((times . (((:timestamp-type scheduled
                                 :timestamp-string "<2018-01-04 Thu 16:05>"
                                 :time ,event-time))))
                    (title . "Test event")
                    (intervals . (10))  ; notify at 16:05 - 10 = 15:55
                    (notify-at . nil))))
      ;; New logic returns all notifications (not time-filtered)
      (let ((new-result (org-wild-notifier--get-notifications-for-event event)))
        (should (= (length new-result) 1))))))

(ert-deftest notifications-multiple-intervals ()
  "Tests that multiple intervals for same event generate multiple notifications."
  (cl-letf* ((time-string "January 4, 2018 15:50")
             ((symbol-function 'current-time)
              (lambda () (apply 'encode-time (parse-time-string time-string)))))
    (let* ((now (current-time))
           (event-time (time-add now 600))  ; 10 minutes from now
           (event `((times . (((:timestamp-type scheduled
                                 :timestamp-string "<2018-01-04 Thu 16:00>"
                                 :time ,event-time))))
                    (title . "Test event")
                    (intervals . (10 5))  ; notify at 15:50 and 15:55
                    (notify-at . nil))))
      ;; Only one notification should fire at exact current time (10 min before)
      (let ((old-result (org-wild-notifier--notifications event)))
        (should (= (length old-result) 1)))
      ;; New logic returns all notifications
      (let ((new-result (org-wild-notifier--get-notifications-for-event event)))
        (should (= (length new-result) 2))))))

(ert-deftest notifications-absolute-vs-relative-timing ()
  "Tests that absolute (notify-at) and relative (interval) notifications work together."
  (cl-letf* ((time-string "January 4, 2018 15:50")
             ((symbol-function 'current-time)
              (lambda () (apply 'encode-time (parse-time-string time-string)))))
    (let* ((now (current-time))
           (event-time (time-add now 600))  ; 10 minutes from now
           (notify-at-time now)  ; absolute notification at exact current time
           (event `((times . (((:timestamp-type scheduled
                                 :timestamp-string "<2018-01-04 Thu 16:00>"
                                 :time ,event-time))))
                    (title . "Test event")
                    (intervals . (10))  ; relative at 15:50
                    (notify-at . (,notify-at-time)))))  ; absolute at 15:50
      ;; New logic should find both the relative and absolute notification
      (let ((new-result (org-wild-notifier--get-notifications-for-event event)))
        ;; Should have 2: one relative, one absolute
        (should (= (length new-result) 2))
        ;; Verify we have both types
        (should (--any? (eq (plist-get it :type) 'relative) new-result))
        (should (--any? (eq (plist-get it :type) 'absolute) new-result))))))

;;; Edge case: repeating timestamp with interval spanning multiple occurrences

(ert-deftest parse-notify-at-multiple-iterations ()
  "Tests repeating timestamp that needs to advance many iterations."
  (cl-letf* ((time-string "February 15, 2018 10:00")
             ((symbol-function 'current-time)
              (lambda () (apply 'encode-time (parse-time-string time-string)))))
    ;; Base is Jan 1, current is Feb 15, daily repeater needs ~45 iterations
    (let ((result (org-wild-notifier--parse-notify-at-timestamp "<2018-01-01 Mon 09:00 +1d>")))
      ;; Result should be Feb 15 or later
      (should (not (time-less-p result (current-time))))
      ;; Result should be at 09:00
      (should (equal (format-time-string "%H:%M" result) "09:00")))))

;;; Edge case: notification at exact boundary of minute

(ert-deftest time-equality-ignores-seconds ()
  "Tests that time comparison ignores seconds within the minute."
  (let* ((time1 (encode-time 0 30 15 4 1 2018))   ; 15:30:00
         (time2 (encode-time 45 30 15 4 1 2018))) ; 15:30:45
    ;; These should be considered equal (same minute)
    (should (org-wild-notifier--time= time1 time2))))

;;; Edge case: event with both scheduled and deadline at different times

(ert-deftest notifications-multiple-event-times ()
  "Tests event with multiple timestamps (e.g., scheduled + deadline)."
  (cl-letf* ((time-string "January 4, 2018 15:50")
             ((symbol-function 'current-time)
              (lambda () (apply 'encode-time (parse-time-string time-string)))))
    (let* ((now (current-time))
           (time1 (time-add now 600))   ; 16:00
           (time2 (time-add now 1200))  ; 16:10
           (event `((times . (((:timestamp-type scheduled
                                 :timestamp-string "<2018-01-04 Thu 16:00>"
                                 :time ,time1)
                               (:timestamp-type deadline
                                 :timestamp-string "<2018-01-04 Thu 16:10>"
                                 :time ,time2))))
                    (title . "Multi-time event")
                    (intervals . (10))
                    (notify-at . nil))))
      ;; Old logic at 15:50 should find notification for 16:00 event
      (let ((old-result (org-wild-notifier--notifications event)))
        (should (= (length old-result) 1)))
      ;; New logic returns all notifications (both timestamps × all intervals)
      (let ((new-result (org-wild-notifier--get-notifications-for-event event)))
        (should (= (length new-result) 2))))))

;;; Edge case: negative intervals (notification after event)

(ert-deftest notifications-negative-interval ()
  "Tests behavior with negative interval (notify after event)."
  (cl-letf* ((time-string "January 4, 2018 15:50")
             ((symbol-function 'current-time)
              (lambda () (apply 'encode-time (parse-time-string time-string)))))
    (let* ((now (current-time))
           (event-time (time-subtract now 600))  ; Event was 10 minutes ago
           (event `((times . (((:timestamp-type scheduled
                                 :timestamp-string "<2018-01-04 Thu 15:40>"
                                 :time ,event-time))))
                    (title . "Past event")
                    (intervals . (-10))  ; Notify 10 minutes AFTER event
                    (notify-at . nil))))
      ;; With negative interval, notify-time = event-time - (-10) = event-time + 10
      ;; So notification should be at 15:40 + 10 = 15:50 (now)
      (let ((new-result (org-wild-notifier--get-notifications-for-event event)))
        ;; This tests that negative intervals work as expected
        (should (= (length new-result) 1))
        ;; Check that the computed notification time is correct (15:50)
        (let ((notify-at (plist-get (car new-result) :notify-at)))
          (should (equal (format-time-string "%H:%M" notify-at) "15:50")))))))

;;; Edge case: very large interval (more than a day)

(ert-deftest notifications-large-interval ()
  "Tests behavior with interval larger than 24 hours."
  (cl-letf* ((time-string "January 4, 2018 15:50")
             ((symbol-function 'current-time)
              (lambda () (apply 'encode-time (parse-time-string time-string)))))
    (let* ((now (current-time))
           ;; Event at now + 1450 min = 15:50 + 24h 10m = ~16:00 next day
           (event-time (time-add now (* 60 1450)))
           (event `((times . (((:timestamp-type scheduled
                                 :timestamp-string "<2018-01-05 Fri 16:00>"
                                 :time ,event-time))))
                    (title . "Tomorrow event")
                    (intervals . (1440))  ; 24 hours = 1440 minutes
                    (notify-at . nil))))
      ;; Notification at event - 1440 min = (now + 1450) - 1440 = now + 10 min
      (let ((new-result (org-wild-notifier--get-notifications-for-event event)))
        ;; New function returns all notifications regardless of time
        (should (= (length new-result) 1))
        ;; Verify the notification time is computed correctly
        ;; notify_at = event_time - 1440 = now + 1450 - 1440 = now + 10
        (let ((notify-at (plist-get (car new-result) :notify-at)))
          ;; Notification should be 10 minutes from now (16:00)
          (should (equal (format-time-string "%H:%M" notify-at) "16:00")))))))

;;; Edge case: interval of exactly 0

(ert-deftest notifications-exact-zero-interval-old-logic ()
  "Tests that old logic handles zero interval correctly."
  (cl-letf* ((time-string "January 4, 2018 16:00")
             ((symbol-function 'current-time)
              (lambda () (apply 'encode-time (parse-time-string time-string)))))
    (let* ((now (current-time))
           (event `((times . (((:timestamp-type scheduled
                                 :timestamp-string "<2018-01-04 Thu 16:00>"
                                 :time ,now))))
                    (title . "Now event")
                    (intervals . (0))
                    (notify-at . nil))))
      ;; Old logic: checks if (current-time) + interval == event-time
      ;; With interval=0: current-time + 0 == current-time, should match
      (let ((old-result (org-wild-notifier--notifications event)))
        (should (= (length old-result) 1))))))

;;; Edge case: timestamp with no year (relative date)
;;; Note: org timestamps always include year, but let's test malformed input

(ert-deftest extract-notify-at-times-malformed-timestamp ()
  "Tests that malformed timestamps don't crash the parser."
  (with-temp-buffer
    (org-mode)
    (insert "* Test heading\n")
    (insert "  :PROPERTIES:\n")
    (insert "  :WILD_NOTIFIER_NOTIFY_AT: not a timestamp\n")
    (insert "  :END:\n")
    (goto-char (point-min))
    (let ((marker (point-marker)))
      ;; Should return nil, not crash
      (should (equal (org-wild-notifier--extract-notify-at-times marker) nil)))))

;;; Edge case: mixed valid and invalid timestamps

(ert-deftest extract-notify-at-times-mixed-valid-invalid ()
  "Tests that valid timestamps are extracted even with garbage text."
  (cl-letf* ((time-string "January 4, 2018 15:00")
             ((symbol-function 'current-time)
              (lambda () (apply 'encode-time (parse-time-string time-string)))))
    (with-temp-buffer
      (org-mode)
      (insert "* Test heading\n")
      (insert "  :PROPERTIES:\n")
      (insert "  :WILD_NOTIFIER_NOTIFY_AT: garbage <2018-01-04 Thu 14:30> more text\n")
      (insert "  :END:\n")
      (goto-char (point-min))
      (let ((marker (point-marker)))
        (let ((result (org-wild-notifier--extract-notify-at-times marker)))
          ;; Should extract the valid timestamp
          (should (= (length result) 1)))))))

;;; Edge case: timestamp at midnight

(ert-deftest parse-notify-at-timestamp-midnight ()
  "Tests parsing timestamp at midnight."
  (cl-letf* ((time-string "January 4, 2018 23:55")
             ((symbol-function 'current-time)
              (lambda () (apply 'encode-time (parse-time-string time-string)))))
    (let ((result (org-wild-notifier--parse-notify-at-timestamp "<2018-01-05 Fri 00:00>")))
      (should (equal (format-time-string "%Y-%m-%d %H:%M" result) "2018-01-05 00:00")))))

;;; Edge case: monthly repeater crossing year boundary

(ert-deftest parse-notify-at-timestamp-monthly-year-boundary ()
  "Tests monthly repeater advancing across year boundary."
  (cl-letf* ((time-string "January 15, 2019 10:00")
             ((symbol-function 'current-time)
              (lambda () (apply 'encode-time (parse-time-string time-string)))))
    ;; Base is Dec 2018, current is Jan 2019
    (let ((result (org-wild-notifier--parse-notify-at-timestamp "<2018-12-01 Sat 09:00 +1m>")))
      ;; Should advance to Jan 2019 or later
      (should (not (time-less-p result (current-time))))
      (should (equal (format-time-string "%H:%M" result) "09:00")))))

;;; Edge case: notification around midnight

(ert-deftest notifications-around-midnight ()
  "Tests notification generation around midnight."
  (cl-letf* ((time-string "January 4, 2018 23:55")
             ((symbol-function 'current-time)
              (lambda () (apply 'encode-time (parse-time-string time-string)))))
    (let* ((now (current-time))
           (event-time (time-add now 600))  ; 00:05 next day
           (event `((times . (((:timestamp-type scheduled
                                 :timestamp-string "<2018-01-05 Fri 00:05>"
                                 :time ,event-time))))
                    (title . "Midnight event")
                    (intervals . (10))  ; notify at 23:55 (now)
                    (notify-at . nil))))
      (let ((new-result (org-wild-notifier--get-notifications-for-event event)))
        (should (= (length new-result) 1))
        ;; Verify notification time is at 23:55
        (let ((notify-at (plist-get (car new-result) :notify-at)))
          (should (equal (format-time-string "%H:%M" notify-at) "23:55")))))))

;;; Test for has-timestamp edge cases

(ert-deftest has-timestamp-day-wide ()
  "Tests that day-wide timestamps are correctly identified as having no time."
  (should-not (org-wild-notifier--has-timestamp "<2018-01-04 Thu>")))

(ert-deftest has-timestamp-with-time ()
  "Tests that timestamps with time are correctly identified."
  (should (org-wild-notifier--has-timestamp "<2018-01-04 Thu 16:00>")))

(ert-deftest has-timestamp-midnight ()
  "Tests that midnight timestamps are correctly identified as having time."
  (should (org-wild-notifier--has-timestamp "<2018-01-04 Thu 00:00>")))

;;; Test for bi-weekly and other multi-unit repeaters

(ert-deftest parse-notify-at-timestamp-biweekly-repeater ()
  "Tests bi-weekly (+2w) repeating timestamps."
  (cl-letf* ((time-string "January 22, 2018 10:00")
             ((symbol-function 'current-time)
              (lambda () (apply 'encode-time (parse-time-string time-string)))))
    ;; Base is Jan 4, current is Jan 22, +2w should advance to Jan 18 then Feb 1
    (let ((result (org-wild-notifier--parse-notify-at-timestamp "<2018-01-04 Thu 09:00 +2w>")))
      (should (not (time-less-p result (current-time))))
      (should (equal (format-time-string "%H:%M" result) "09:00")))))

;;; Test times data structure edge case - nil vs empty

(ert-deftest notifications-times-nil-not-list ()
  "Tests behavior when times value is nil instead of list containing nil."
  (let ((event '((times . nil)  ; nil directly, not (nil)
                 (title . "Nil times event")
                 (intervals . (10))
                 (notify-at . nil))))
    ;; Should not crash
    (should (equal (org-wild-notifier--get-notifications-for-event event) nil))))

;;; Test full integration with fixture file

(ert-deftest upcoming-notifications-integration-basic ()
  "Integration test: get upcoming notifications from real org file."
  (cl-letf* ((org-agenda-files (list (expand-file-name "planning.org" "fixtures")))
             (org-todo-keywords (list "TODO" "IN PROGRESS" "DONE"))
             (time-string "January 4, 2018 15:50")
             ((symbol-function 'current-time)
              (lambda () (apply 'encode-time (parse-time-string time-string))))
             (org-agenda-start-day (time-to-days (current-time)))
             ((symbol-function 'org-today)
              (lambda () (time-to-days (current-time))))
             (org-wild-notifier-keyword-whitelist '("TODO"))
             (org-wild-notifier-keyword-blacklist nil)
             (org-wild-notifier-tags-whitelist nil)
             (org-wild-notifier-tags-blacklist nil)
             (org-wild-notifier-predicate-whitelist nil)
             (org-wild-notifier-predicate-blacklist '(org-wild-notifier-done-keywords-predicate)))
    ;; Get all upcoming notifications (no time filter - new API takes no args)
    (let ((result (org-wild-notifier-get-upcoming-notifications)))
      ;; Should have at least some notifications
      (should (> (length result) 0))
      ;; All results should be plists with required keys
      (--each result
        (should (plist-get it :notify-at))
        (should (plist-get it :title))
        (should (plist-get it :type))))))

;;; Test that sync and async logic produce same events

(ert-deftest gather-events-sync-returns-valid-structure ()
  "Tests that gather-events-sync returns properly structured events."
  (cl-letf* ((org-agenda-files (list (expand-file-name "planning.org" "fixtures")))
             (org-todo-keywords (list "TODO" "IN PROGRESS" "DONE"))
             (time-string "January 4, 2018 15:50")
             ((symbol-function 'current-time)
              (lambda () (apply 'encode-time (parse-time-string time-string))))
             (org-agenda-start-day (time-to-days (current-time)))
             ((symbol-function 'org-today)
              (lambda () (time-to-days (current-time))))
             (org-wild-notifier-keyword-whitelist '("TODO"))
             (org-wild-notifier-keyword-blacklist nil)
             (org-wild-notifier-tags-whitelist nil)
             (org-wild-notifier-tags-blacklist nil)
             (org-wild-notifier-predicate-whitelist nil)
             (org-wild-notifier-predicate-blacklist '(org-wild-notifier-done-keywords-predicate)))
    (let ((events (org-wild-notifier--gather-events-sync)))
      ;; Should return a list of events
      (should (listp events))
      ;; Each event should have the required structure
      (--each events
        (should (assoc 'times it))
        (should (assoc 'title it))
        (should (assoc 'intervals it))
        (should (assoc 'notify-at it))))))

;;; Test edge case: notification time computation

(ert-deftest notifications-time-computation ()
  "Tests that notification times are correctly computed."
  (cl-letf* ((time-string "January 4, 2018 15:50")
             ((symbol-function 'current-time)
              (lambda () (apply 'encode-time (parse-time-string time-string)))))
    (let* ((now (current-time))
           ;; Event at 16:10
           (event-time (time-add now 1200))  ; 20 min from now
           (event `((times . (((:timestamp-type scheduled
                                 :timestamp-string "<2018-01-04 Thu 16:10>"
                                 :time ,event-time))))
                    (title . "Boundary test")
                    (intervals . (10))  ; notify at 16:00
                    (notify-at . nil))))
      (let ((result (org-wild-notifier--get-notifications-for-event event)))
        (should (= (length result) 1))
        ;; Verify notification is at 16:00 (event 16:10 - 10 min)
        (let ((notify-at (plist-get (car result) :notify-at)))
          (should (equal (format-time-string "%H:%M" notify-at) "16:00")))))))

;;; Test that repeating notify-at matches on the correct day

(ert-deftest parse-notify-at-repeating-same-day ()
  "Tests repeating timestamp that lands on the same day as current."
  (cl-letf* ((time-string "January 4, 2018 15:00")
             ((symbol-function 'current-time)
              (lambda () (apply 'encode-time (parse-time-string time-string)))))
    ;; Base is Jan 1 at 16:00, repeating daily
    ;; Current is Jan 4 at 15:00
    ;; Should advance to Jan 4 at 16:00 (same day, later time)
    (let ((result (org-wild-notifier--parse-notify-at-timestamp "<2018-01-01 Mon 16:00 +1d>")))
      (should (equal (format-time-string "%Y-%m-%d %H:%M" result) "2018-01-04 16:00")))))

;;; Test for verifying consistency between check-event and get-notifications-for-event

(ert-deftest check-event-vs-get-notifications-consistency ()
  "Tests that check-event and get-notifications-for-event are consistent."
  (cl-letf* ((time-string "January 4, 2018 15:50")
             ((symbol-function 'current-time)
              (lambda () (apply 'encode-time (parse-time-string time-string)))))
    (let* ((now (current-time))
           (event-time (time-add now 600))  ; 10 minutes from now
           (event `((times . (((:timestamp-type scheduled
                                 :timestamp-string "<2018-01-04 Thu 16:00>"
                                 :time ,event-time))))
                    (title . "Test event")
                    (intervals . (10))
                    (notify-at . nil))))
      ;; check-event finds notifications at exact current time
      (let ((check-result (org-wild-notifier--check-event event)))
        (should (= (length check-result) 1)))
      ;; get-notifications-for-event returns all notifications
      (let ((new-result (org-wild-notifier--get-notifications-for-event event)))
        (should (= (length new-result) 1))))))

;;; Tests for time comparison edge cases (format string truncation)

(ert-deftest time-comparison-across-seconds ()
  "Tests that time= comparison works across different second values."
  ;; The old logic uses format-time-string "%d:%H:%M" which truncates seconds
  (let ((t1 (encode-time 0 30 15 4 1 2018))
        (t2 (encode-time 30 30 15 4 1 2018))
        (t3 (encode-time 59 30 15 4 1 2018)))
    ;; All should be equal (same minute)
    (should (org-wild-notifier--time= t1 t2))
    (should (org-wild-notifier--time= t2 t3))
    (should (org-wild-notifier--time= t1 t3))))

(ert-deftest time-comparison-different-minutes ()
  "Tests that time= correctly identifies different minutes."
  (let ((t1 (encode-time 59 30 15 4 1 2018))  ; 15:30:59
        (t2 (encode-time 0 31 15 4 1 2018)))  ; 15:31:00
    ;; Should be different (different minutes)
    (should-not (org-wild-notifier--time= t1 t2))))

(ert-deftest time-comparison-same-time-different-days ()
  "Tests that time= correctly handles same time on different days."
  (let ((t1 (encode-time 0 30 15 4 1 2018))
        (t2 (encode-time 0 30 15 5 1 2018)))
    ;; Same time but different day - should NOT be equal
    (should-not (org-wild-notifier--time= t1 t2))))

;;; Test for potential rounding/truncation issues

(ert-deftest timestamp-within-interval-at-boundary ()
  "Tests timestamp-within-interval-p at minute boundary."
  (cl-letf* ((time-string "January 4, 2018 15:50:30")  ; 30 seconds into the minute
             ((symbol-function 'current-time)
              (lambda () (apply 'encode-time (parse-time-string time-string)))))
    ;; Event at 16:00:00 with 10 minute interval
    ;; Notification should be at 15:50:00
    ;; Current time is 15:50:30
    ;; Should still match because comparison is at minute granularity
    (let ((event-time (encode-time 0 0 16 4 1 2018)))
      (should (org-wild-notifier--timestamp-within-interval-p event-time 10)))))

;;; Test that the new functions correctly compute notification times

(ert-deftest get-notification-time-calculation ()
  "Tests that notification time is correctly calculated as event_time - interval."
  (cl-letf* ((time-string "January 4, 2018 15:00")
             ((symbol-function 'current-time)
              (lambda () (apply 'encode-time (parse-time-string time-string)))))
    (let* ((now (current-time))
           (event-time (time-add now 1800))  ; 30 minutes from now (15:30)
           (event `((times . (((:timestamp-type scheduled
                                 :timestamp-string "<2018-01-04 Thu 15:30>"
                                 :time ,event-time))))
                    (title . "Test event")
                    (intervals . (15))  ; notify at 15:15
                    (notify-at . nil))))
      (let ((result (org-wild-notifier--get-notifications-for-event event)))
        (should (= (length result) 1))
        ;; Verify the notification time is 15:15
        (let ((notify-at (plist-get (car result) :notify-at)))
          (should (equal (format-time-string "%H:%M" notify-at) "15:15")))))))

;;; Test consistency between the two notification checking approaches
;;; when notification falls exactly at current time

(ert-deftest notification-timing-consistency-at-exact-minute ()
  "Tests that old and new approaches agree when notification is at exact current minute."
  (cl-letf* ((time-string "January 4, 2018 15:50:00")
             ((symbol-function 'current-time)
              (lambda () (apply 'encode-time (parse-time-string time-string)))))
    (let* ((now (current-time))
           (event-time (time-add now 600))  ; Event at 16:00:00
           (event `((times . (((:timestamp-type scheduled
                                 :timestamp-string "<2018-01-04 Thu 16:00>"
                                 :time ,event-time))))
                    (title . "Test")
                    (intervals . (10))
                    (notify-at . nil))))
      ;; Old approach: check if notification matches current time
      (let ((old-notifications (org-wild-notifier--notifications event)))
        ;; New approach returns all notifications
        (let ((new-notifications (org-wild-notifier--get-notifications-for-event event)))
          ;; Both should find exactly one notification
          (should (= (length old-notifications) 1))
          (should (= (length new-notifications) 1)))))))

;;; Edge case: very short intervals (less than 1 minute)
;;; Note: The system uses minutes, so sub-minute intervals would round

(ert-deftest notification-small-interval ()
  "Tests behavior with small intervals (1 minute)."
  (cl-letf* ((time-string "January 4, 2018 15:59:30")
             ((symbol-function 'current-time)
              (lambda () (apply 'encode-time (parse-time-string time-string)))))
    (let* ((now (current-time))
           (event-time (time-add now 90))  ; Event at 16:01:00 (90 seconds from now)
           (event `((times . (((:timestamp-type scheduled
                                 :timestamp-string "<2018-01-04 Thu 16:01>"
                                 :time ,event-time))))
                    (title . "Test")
                    (intervals . (1))  ; 1 minute before = 16:00
                    (notify-at . nil))))
      (let ((result (org-wild-notifier--get-notifications-for-event event)))
        ;; Should find the notification
        (should (= (length result) 1))
        ;; Verify notification time is 16:00
        (let ((notify-at (plist-get (car result) :notify-at)))
          (should (equal (format-time-string "%H:%M" notify-at) "16:00")))))))
