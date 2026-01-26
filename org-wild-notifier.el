;;; org-wild-notifier.el --- Customizable org-agenda notifications -*- lexical-binding: t -*-


;; Copyright (C) 2017 Artem Khramov

;; Author: Artem Khramov <akhramov+emacs@pm.me>
;; Created: 6 Jan 2017
;; Version: 0.7.0
;; Package-Requires: ((alert "1.2") (async "1.9.3") (dash "2.18.0") (emacs "27.1"))
;; Keywords: calendar, convenience
;; URL: https://github.com/emacsorphanage/org-wild-notifier.el

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
;;
;; To perform a one-time check use `org-wild-notifier-check'
;; function.
;; To enable timer-based notifications please use
;;`org-wild-notifier-mode'.
;; Notification times can be customized either globally (for all org
;; entries) through `org-wild-notifier-alert-time' variable or on per
;; org entry basis using `WILD_NOTIFIER_NOTIFY_BEFORE` property, which
;; in turn is customizable via
;; `org-wild-notifier-alert-times-property' variable.
;; By default you get notifications about TODO events only.  To
;; customize that behavior please use
;; `org-wild-notifier-keyword-whitelist' variable.  In contrary, if
;; you don't want to receive notifications regarding certain events,
;; you can use `org-wild-notifier-keyword-blacklist' variable.

;;; Code:

(require 'dash)
(require 'alert)
(require 'async)
(require 'org-agenda)
(require 'org-duration)
(require 'cl-lib)

(defgroup org-wild-notifier nil
  "Customization options for org-wild-notifier."
  :group 'org)

(defcustom org-wild-notifier-alert-time '(10)
  "Time in minutes to get a notification about upcoming event.
Cannot be less than 1."
  :package-version '(org-wild-notifier . "0.1.0")
  :group 'org-wild-notifier
  :type '(choice (integer :tag "Notify once")
                 (repeat integer)))

(defcustom org-wild-notifier-alert-times-property "WILD_NOTIFIER_NOTIFY_BEFORE"
  "Property to add additional notifications to an event."
  :package-version '(org-wild-notifier . "0.1.0")
  :group 'org-wild-notifier
  :type 'string)

(defcustom org-wild-notifier-notify-at-property "WILD_NOTIFIER_NOTIFY_AT"
  "Property for absolute notification times.
Value should be one or more org timestamps, e.g.:
  <2024-01-21 14:30>
  <2024-01-21 14:30> <2024-01-22 09:00>
  <2024-01-21 14:30 +1d>  ; repeating daily"
  :package-version '(org-wild-notifier . "0.6.0")
  :group 'org-wild-notifier
  :type 'string)

(defcustom org-wild-notifier-notification-title "Agenda"
  "Notifications title."
  :package-version '(org-wild-notifier . "0.1.0")
  :group 'org-wild-notifier
  :type 'string)

(defcustom org-wild-notifier-notification-icon nil
  "Path to notification icon file."
  :package-version '(org-wild-notifier . "0.4.1")
  :group 'org-wild-notifier
  :type 'string)

(defcustom org-wild-notifier-keyword-whitelist nil
  "Receive notifications for these keywords only.
Leave this variable blank if you do not want to filter anything."
  :package-version '(org-wild-notifier . "0.2.2")
  :group 'org-wild-notifier
  :type '(repeat string))

(defcustom org-wild-notifier-keyword-blacklist nil
  "Never receive notifications for these keywords."
  :package-version '(org-wild-notifier . "0.2.2")
  :group 'org-wild-notifier
  :type '(repeat string))

(defcustom org-wild-notifier-tags-whitelist nil
  "Receive notifications for these tags only.
Leave this variable blank if you do not want to filter anything."
  :package-version '(org-wild-notifier . "0.3.1")
  :group 'org-wild-notifier
  :type '(repeat string))

(defcustom org-wild-notifier-tags-blacklist nil
  "Never receive notifications for these tags."
  :package-version '(org-wild-notifier . "0.3.1")
  :group 'org-wild-notifier
  :type '(repeat string))

(defcustom org-wild-notifier-entries '("DEADLINE" "SCHEDULED" "TIMESTAMP")
  "Timestamp entry types that trigger notifications.
Only timestamps of these types will be considered for notifications.
Valid values are \"DEADLINE\", \"SCHEDULED\", and \"TIMESTAMP\"."
  :package-version '(org-wild-notifier . "0.3.1")
  :group 'org-wild-notifier
  :type '(repeat string))

(defcustom org-wild-notifier-entries-property "WILD_NOTIFIER_ENTRIES"
  "Property to override which timestamp types trigger notifications.
When set on an org entry (or inherited from parent headings or file),
this overrides `org-wild-notifier-entries' for that entry.
Value should be space-separated timestamp types, e.g., \"DEADLINE SCHEDULED\"."
  :package-version '(org-wild-notifier . "0.7.0")
  :group 'org-wild-notifier
  :type 'string)

(defcustom org-wild-notifier-display-time-format-string "%I:%M %p"
  "Format string for `format-time-string' when displaying times."
  :package-version '(org-wild-notifier . "0.5.0")
  :group 'org-wild-notifier
  :type 'string)

(defcustom org-wild-notifier-predicate-whitelist nil
  "Receive notifications for events matching these predicates only.
Each function should take an event POM and return non-nil iff that event should
trigger a notification.  Leave this variable blank if you do not want to filter
anything."
  :package-version '(org-wild-notifier . "0.5.0")
  :group 'org-wild-notifier
  :type '(function))

(defcustom org-wild-notifier-additional-environment-regexes nil
  "Additional regular expressions for `async-inject-environment'.
These are passed to the async command when checking notifications."
  :package-version '(org-wild-notifier . "0.5.0")
  :group 'org-wild-notifier
  :type '(string))

(defcustom org-wild-notifier-predicate-blacklist
  '(org-wild-notifier-done-keywords-predicate)
  "Never receive notifications for events matching these predicates.
Each function should take an event POM and return non-nil iff that event should
not trigger a notification."
  :package-version '(org-wild-notifier . "0.5.0")
  :group 'org-wild-notifier
  :type '(function))

(defcustom org-wild-notifier--alert-severity 'medium
  "Severity of the alert.
Options: high, medium, low."
  :package-version '(org-wild-notifier . "0.3.1")
  :group 'org-wild-notifier
  :type 'symbol
  :options '(high medium low))

(defcustom org-wild-notifier-extra-alert-plist nil
  "Additional arguments that should be passed to invocations of `alert'."
  :package-version "v0.5.0"
  :group 'org-wild-notifier
  :type 'plist)

(defcustom org-wild-notifier-day-wide-alert-times nil
  "List of time strings when alerts for day-wide events should trigger.
Each string should be in HH:MM format (e.g., \"09:00\", \"14:30\").
Day-wide events (those without a specific time) will trigger notifications
at each of these times."
  :package-version '(org-wild-notifier . "0.5.0")
  :group 'org-wild-notifier
  :type '(repeat string))

(defcustom org-wild-notifier-show-any-overdue-with-day-wide-alerts t
  "Show any overdue TODO items along with day-wide alerts.
When non-nil, overdue items are included in day-wide alert notifications."
  :package-version '(org-wild-notifier . "0.5.0")
  :group 'org-wild-notifier
  :type 'boolean)

(defvar org-wild-notifier--timer nil
  "Timer value.")

(defvar org-wild-notifier--process nil
  "Currently-running async process.")

(defvar org-wild-notifier--agenda-buffer-name "*org wild notifier affairs*"
  "A name for temporary `org-agenda' buffer.")

(defvar org-wild-notifier--last-check-time (seconds-to-time 0)
  "Last time checked for events.")

(defun org-wild-notifier--time= (&rest list)
  "Compare timestamps.
Comparison is performed by converted each element of LIST onto string
in order to ignore seconds."
  (->> list
       (--map (format-time-string "%d:%H:%M" it))
       (-uniq)
       (length)
       (= 1)))

(defun org-wild-notifier--today ()
  "Get the timestamp for the beginning of current day."
  (apply 'encode-time
         (append '(0 0 0) (nthcdr 3 (decode-time (current-time))))))

(defun org-wild-notifier--timestamp-within-interval-p (timestamp interval)
  "Check whether TIMESTAMP is within notification INTERVAL."
  (org-wild-notifier--time=
   (time-add (current-time) (seconds-to-time (* 60 interval)))
   timestamp))

(defun org-wild-notifier--notification-due-now-p (notification)
  "Return non-nil if NOTIFICATION should fire at the current minute."
  (org-wild-notifier--time= (plist-get notification :notify-at) (current-time)))

(defun org-wild-notifier--get-current-notifications (event)
  "Get notifications for EVENT that are due at the current minute.
Returns a list of notification plists filtered to those due now."
  (->> (org-wild-notifier--get-notifications-for-event event)
       (--filter (org-wild-notifier--notification-due-now-p it))))

(defun org-wild-notifier--notifications (event)
  "Get notifications for given EVENT that are due now.
Returns a list of notification plists.
DEPRECATED: Use `org-wild-notifier--get-current-notifications' instead."
  (org-wild-notifier--get-current-notifications event))

(defun org-wild-notifier--has-timestamp (s)
  "Check whether S has a time component in its timestamp."
  (string-match org-ts-regexp0 s)
  (match-beginning 7))

(defun org-wild-notifier--filter-day-wide-events (times)
  "Filter out day-wide events (those without a time component) from TIMES.
TIMES is a list of time plists with :timestamp-string."
  (--filter (org-wild-notifier--has-timestamp (plist-get it :timestamp-string)) times))

(defun org-wild-notifier--time-left (seconds)
  "Human-friendly representation for SECONDS."
  (-> seconds
       (pcase
         ((pred (>= 0)) "right now")
         ((pred (>= 3600)) "in %M")
         (_ "in %H %M"))

       (format-seconds seconds)))

(defun org-wild-notifier--get-hh-mm-from-org-time-string (time-string)
  "Convert org TIME-STRING into display format."
  (format-time-string
   org-wild-notifier-display-time-format-string
   (encode-time (org-parse-time-string time-string))))

(defun org-wild-notifier--notification-text (str-interval event)
  "For given STR-INTERVAL list and EVENT get notification wording."
  (format "%s at %s (%s)"
          (cdr (assoc 'title event))
          (org-wild-notifier--get-hh-mm-from-org-time-string (car str-interval))
          (org-wild-notifier--time-left (* 60 (cdr str-interval)))))

(defun org-wild-notifier-get-minutes-into-day (time)
  "Return the number of minutes from midnight for TIME string."
  (org-duration-to-minutes (org-get-time-of-day time t)))

(defun org-wild-notifier-get-hours-minutes-from-time (time-string)
  "Parse TIME-STRING and return a list of (hours minutes)."
  (let ((total-minutes (truncate (org-wild-notifier-get-minutes-into-day time-string))))
    (list (/ total-minutes 60)
          (mod total-minutes 60))))

(defun org-wild-notifier-set-hours-minutes-for-time (time hours minutes)
  "Return TIME with HOURS and MINUTES set, preserving the date."
  (cl-destructuring-bind (_s _m _h day month year dow dst utcoff) (decode-time time)
    (encode-time 0 minutes hours day month year dow dst utcoff)))

(defun org-wild-notifier-current-time-matches-time-of-day-string (time-of-day-string)
  "Return non-nil if current time matches TIME-OF-DAY-STRING."
  (let ((now (current-time)))
    (org-wild-notifier--time=
     now
     (apply 'org-wild-notifier-set-hours-minutes-for-time
            now
            (org-wild-notifier-get-hours-minutes-from-time time-of-day-string)))))

(defun org-wild-notifier-current-time-is-day-wide-time ()
  "Return non-nil if current time matches any day-wide alert time."
  (--any (org-wild-notifier-current-time-matches-time-of-day-string it)
         org-wild-notifier-day-wide-alert-times))

(defun org-wild-notifier--day-wide-alert-times-for-today ()
  "Get Emacs time values for all day-wide alert times for today."
  (let ((now (current-time)))
    (--map (apply 'org-wild-notifier-set-hours-minutes-for-time
                  now
                  (org-wild-notifier-get-hours-minutes-from-time it))
           org-wild-notifier-day-wide-alert-times)))

(defun org-wild-notifier-day-wide-notifications (events)
  "Return unique day-wide notification texts for EVENTS."
  (->> events
       (-filter 'org-wild-notifier-display-as-day-wide-event)
       (-map 'org-wild-notifier--day-wide-notification-text)
       (-uniq)))

(defun org-wild-notifier-display-as-day-wide-event (event)
  "Return non-nil if EVENT should display as a day-wide event.
`org-wild-notifier-event-has-any-passed-time' is required regardless of
`org-wild-notifier-show-any-overdue-with-day-wide-alerts' because the events
list can include events scheduled tomorrow.  We only alert for today."
  (and (org-wild-notifier-event-has-any-passed-time event)
      (or org-wild-notifier-show-any-overdue-with-day-wide-alerts
           (org-wild-notifier-event-has-any-day-wide-timestamp event))))

(defun org-wild-notifier-event-has-any-day-wide-timestamp (event)
  "Return non-nil if EVENT has any day-wide timestamp (no time component)."
  (--any (not (org-wild-notifier--has-timestamp (plist-get it :timestamp-string)))
         (cadr (assoc 'times event))))

(defun org-wild-notifier-event-has-any-passed-time (event)
  "Return non-nil if EVENT has any timestamp that has already passed."
  (--any (time-less-p (plist-get it :time) (current-time))
         (cadr (assoc 'times event))))

(defun org-wild-notifier--day-wide-notification-text (event)
  "For given STR-INTERVAL list and EVENT get notification wording."
  (format "%s is due or scheduled today"
          (cdr (assoc 'title event))))

(defun org-wild-notifier--format-notification (notification)
  "Format NOTIFICATION plist as a human-readable message string."
  (let ((type (plist-get notification :type))
        (title (plist-get notification :title))
        (event-time-string (plist-get notification :event-time-string))
        (minutes-before (plist-get notification :minutes-before)))
    (pcase type
      ('absolute (format "%s (scheduled reminder)" title))
      ('day-wide (format "%s is due or scheduled today" title))
      (_ (format "%s at %s (%s)"
                 title
                 (org-wild-notifier--get-hh-mm-from-org-time-string event-time-string)
                 (org-wild-notifier--time-left (* 60 minutes-before)))))))

(defun org-wild-notifier--check-notify-at (event)
  "Check if any absolute notification times in EVENT match now.
Returns list of notification plists.
DEPRECATED: Use `org-wild-notifier--get-current-notifications' instead."
  (--filter (eq (plist-get it :type) 'absolute)
            (org-wild-notifier--get-current-notifications event)))

(defun org-wild-notifier--check-event (event)
  "Get notifications for given EVENT that are due now.
Returns a list of notification messages."
  (->> (org-wild-notifier--get-current-notifications event)
       (--map (org-wild-notifier--format-notification it))))

(defun org-wild-notifier--get-tags (marker)
  "Retrieve tags of MARKER."
  (-> (org-entry-get marker "TAGS")
      (or "")
      (org-split-string  ":")))

(defun org-wild-notifier--whitelist-predicates ()
  "Return a list of predicate functions for whitelist filtering."
  (->> `([,org-wild-notifier-keyword-whitelist
          (lambda (it)
            (-contains-p org-wild-notifier-keyword-whitelist
                         (org-entry-get it "TODO")))]

         [,org-wild-notifier-tags-whitelist
          (lambda (it)
            (-intersection org-wild-notifier-tags-whitelist
                           (org-wild-notifier--get-tags it)))]

         [,org-wild-notifier-predicate-whitelist
          (lambda (marker)
            (--some? (funcall it marker) org-wild-notifier-predicate-whitelist))])
       (--filter (aref it 0))
       (--map (aref it 1))))

(defun org-wild-notifier--blacklist-predicates ()
  "Return a list of predicate functions for blacklist filtering."
  (->> `([,org-wild-notifier-keyword-blacklist
          (lambda (it)
            (-contains-p org-wild-notifier-keyword-blacklist
                         (org-entry-get it "TODO")))]

         [,org-wild-notifier-tags-blacklist
          (lambda (it)
            (-intersection org-wild-notifier-tags-blacklist
                           (org-wild-notifier--get-tags it)))]

         [,org-wild-notifier-predicate-blacklist
          (lambda (marker)
            (--some? (funcall it marker) org-wild-notifier-predicate-blacklist))])
       (--filter (aref it 0))
       (--map (aref it 1))))

(defun org-wild-notifier-done-keywords-predicate (marker)
  "Return non-nil if MARKER points to a heading with a done keyword."
  (with-current-buffer (marker-buffer marker)
    (save-excursion
      (goto-char (marker-position marker))
      (member (nth 2 (org-heading-components)) org-done-keywords))))

(defun org-wild-notifier--apply-whitelist (markers)
  "Apply whitelist to MARKERS."
  (-if-let (whitelist-predicates (org-wild-notifier--whitelist-predicates))
      (-> (apply '-orfn whitelist-predicates)
          (-filter markers))
    markers))

(defun org-wild-notifier--apply-blacklist (markers)
  "Apply blacklist to MARKERS."
  (-if-let (blacklist-predicates (org-wild-notifier--blacklist-predicates))
      (-> (apply '-orfn blacklist-predicates)
          (-remove markers))
    markers))

(defconst org-wild-notifier-default-environment-regex
  (macroexpand
   `(rx string-start
        (or ,@(mapcar (lambda (literal) (list 'literal literal))
                (list
                 "org-agenda-files"
                 "load-path"
                 "org-todo-keywords"
                 "org-wild-notifier-alert-time"
                 "org-wild-notifier-keyword-whitelist"
                 "org-wild-notifier-keyword-blacklist"
                 "org-wild-notifier-tags-whitelist"
                 "org-wild-notifier-tags-blacklist"
                 "org-wild-notifier-predicate-whitelist"
                 "org-wild-notifier-predicate-blacklist"
                 "org-wild-notifier-notify-at-property"
                 "org-wild-notifier-entries"
                 "org-wild-notifier-entries-property")))
        string-end)))


(defun org-wild-notifier-environment-regex ()
  "Return a regex matching all environment variables to inject."
  (macroexpand
   `(rx (or
         ,@(mapcar (lambda (regexp) (list 'regexp regexp))
                   (cons org-wild-notifier-default-environment-regex
                         org-wild-notifier-additional-environment-regexes))))))

(defun org-wild-notifier--retrieve-events ()
  "Get events from agenda view."
  `(lambda ()
    (setf org-agenda-use-time-grid nil)
    (setf org-agenda-compact-blocks t)
    ,(async-inject-variables (org-wild-notifier-environment-regex))

    (package-initialize)
    (require 'org-wild-notifier)

    (org-agenda-list 2 (org-read-date nil nil "today"))

    (->> (org-split-string (buffer-string) "\n")
         (--map (plist-get
                 (org-fix-agenda-info (text-properties-at 0 it))
                 'org-marker))
         (-non-nil)
         (org-wild-notifier--apply-whitelist)
         (org-wild-notifier--apply-blacklist)
         (-map 'org-wild-notifier--gather-info))))

(defun org-wild-notifier--notify (event-msg)
  "Notify about an event using `alert' library.
EVENT-MSG is a string representation of the event."
  (apply
   'alert event-msg
   :icon org-wild-notifier-notification-icon
   :title org-wild-notifier-notification-title
   :severity org-wild-notifier--alert-severity
   :category 'org-wild-notifier
   org-wild-notifier-extra-alert-plist))

(defun org-wild-notifier--timestamp-parse (timestamp)
  "Parse TIMESTAMP and return time value."
  (let ((parsed (org-parse-time-string timestamp))
        (today (format-time-string "<%Y-%m-%d>")))
    ;; seconds-to-time returns also milliseconds and nanoseconds so we
    ;; have to "trim" the list
    (butlast
     (seconds-to-time
      (time-add
       ;; we get the cycled absolute day (not hour and minutes)
       (org-time-from-absolute (org-closest-date timestamp today 'past))
       ;; so we have to add the minutes too
       (+ (* (decoded-time-hour parsed) 3600)
          (* (decoded-time-minute parsed) 60))))
     2)))

(defun org-wild-notifier--get-entries-for-marker (marker)
  "Get the list of timestamp entry types to check for MARKER.
First checks for a per-entry override via `org-wild-notifier-entries-property'
\(with inheritance), then falls back to `org-wild-notifier-entries'."
  (if-let ((prop-value (org-entry-get marker org-wild-notifier-entries-property t)))
      (split-string prop-value)
    org-wild-notifier-entries))

(defun org-wild-notifier--extract-time (marker)
  "Extract timestamps from MARKER.
Returns a list of plists, each containing:
  :timestamp-type - symbol: deadline, scheduled, or timestamp
  :timestamp-string - the original org timestamp string
  :time - parsed time in Emacs time format"
  (-non-nil
   (--map
    (let ((org-timestamp (org-entry-get marker it)))
      (and org-timestamp
           (list :timestamp-type (intern (downcase it))
                 :timestamp-string org-timestamp
                 :time (org-wild-notifier--timestamp-parse org-timestamp))))
    (org-wild-notifier--get-entries-for-marker marker))))

(defun org-wild-notifier--extract-title (marker)
  "Extract event title from MARKER.
MARKER acts like the event's identifier."
  (org-with-point-at marker
    (-let (((_lvl _reduced-lvl _todo _priority title _tags)
            (org-heading-components)))
      title)))

(defun org-wild-notifier--extract-notication-intervals (marker)
  "Extract notification intervals from the event's properties.
MARKER acts like the event's identifier.  Resulting list also contains
standard notification interval (`org-wild-notifier-alert-time')."
  `(,@(-flatten (list org-wild-notifier-alert-time))
    ,@(-map 'string-to-number
           (org-entry-get-multivalued-property
            marker
            org-wild-notifier-alert-times-property))))

(defun org-wild-notifier--parse-notify-at-timestamp (timestamp)
  "Parse TIMESTAMP for NOTIFY_AT property.
Unlike `org-wild-notifier--timestamp-parse', this parses the timestamp
literally using `current-time' as the reference for repeating timestamps,
rather than the real system time."
  (let* ((parsed (org-parse-time-string timestamp))
         (base-time (encode-time 0
                                 (decoded-time-minute parsed)
                                 (decoded-time-hour parsed)
                                 (decoded-time-day parsed)
                                 (decoded-time-month parsed)
                                 (decoded-time-year parsed))))
    ;; Handle repeating timestamps by finding the closest occurrence to now
    (if (string-match "\\+[0-9]+[dwmy]" timestamp)
        ;; Repeating timestamp - find closest occurrence to now
        (let* ((repeat-match (match-string 0 timestamp))
               (repeat-num (string-to-number (substring repeat-match 1 -1)))
               (repeat-unit (substring repeat-match -1))
               (current (current-time))
               (candidate base-time))
          ;; Advance candidate until it's >= current-time or close to it
          (while (time-less-p candidate current)
            (pcase repeat-unit
              ("d" (setq candidate (time-add candidate (* repeat-num 86400))))
              ("w" (setq candidate (time-add candidate (* repeat-num 7 86400))))
              ("m" (let ((decoded (decode-time candidate)))
                     (setf (decoded-time-month decoded)
                           (+ (decoded-time-month decoded) repeat-num))
                     (setq candidate (encode-time decoded))))
              ("y" (let ((decoded (decode-time candidate)))
                     (setf (decoded-time-year decoded)
                           (+ (decoded-time-year decoded) repeat-num))
                     (setq candidate (encode-time decoded))))))
          candidate)
      ;; Non-repeating timestamp - return as-is
      base-time)))

(defun org-wild-notifier--extract-notify-at-times (marker)
  "Extract absolute notification timestamps from MARKER.
Returns list of parsed time values for any org timestamps found
in the WILD_NOTIFIER_NOTIFY_AT property."
  (when-let ((prop-value (org-entry-get marker org-wild-notifier-notify-at-property)))
    (let ((timestamps nil)
          (start 0))
      (while (string-match org-ts-regexp prop-value start)
        (let ((matched-ts (match-string 0 prop-value))
              (match-end-pos (match-end 0)))
          (push (org-wild-notifier--parse-notify-at-timestamp matched-ts) timestamps)
          (setq start match-end-pos)))
      (nreverse timestamps))))

(defun org-wild-notifier--gather-info (marker)
  "Collect information about an event.
MARKER acts like event's identifier."
  `((marker . ,marker)
    (times . (,(org-wild-notifier--extract-time marker)))
    (title . ,(org-wild-notifier--extract-title marker))
    (intervals . ,(org-wild-notifier--extract-notication-intervals marker))
    (notify-at . ,(org-wild-notifier--extract-notify-at-times marker))))

(defun org-wild-notifier--stop ()
  "Stop the notification timer and cancel any in-progress check."
  (-some-> org-wild-notifier--timer (cancel-timer))
  (when org-wild-notifier--process
    (interrupt-process org-wild-notifier--process)
    (setq org-wild-notifier--process nil)))

(defun org-wild-notifier--start ()
  "Start the notification timer.  Cancel old one, if any.
Timer is scheduled on the beginning of every minute, so for
smoother experience this function also runs a check without timer."
  (org-wild-notifier--stop)
  (org-wild-notifier-check)

  (--> (format-time-string "%H:%M" (time-add (current-time) 60))
       (run-at-time it 60 'org-wild-notifier-check)
       (setf org-wild-notifier--timer it)))

(defun org-wild-notifier--check-events (events)
  "Process EVENTS and send notifications for those due now."
  (setq org-wild-notifier--process nil)
  (-each
      (->> events
           (-map 'org-wild-notifier--check-event)
           (-flatten)
           (-uniq))
    'org-wild-notifier--notify)
  (setq org-wild-notifier--last-check-time (current-time)))

;;;###autoload
(defun org-wild-notifier-check ()
  "Parse agenda view and notify about upcoming events.

Do nothing if a check is already in progress in the background."
  (interactive)
  (unless (and org-wild-notifier--process
               (process-live-p org-wild-notifier--process))
    (setq org-wild-notifier--process
          (let ((default-directory user-emacs-directory)
                (async-prompt-for-password nil)
                (async-process-noquery-on-exit t))
            (async-start
             (org-wild-notifier--retrieve-events)
             'org-wild-notifier--check-events)))))

;;;###autoload
(define-minor-mode org-wild-notifier-mode
  "Toggle org notifications globally.
When enabled parses your agenda once a minute and emits notifications
if needed."
  :global
  :lighter "Org Wild Notifier"
  (if org-wild-notifier-mode
      (org-wild-notifier--start)
    (org-wild-notifier--stop)))

;;;###autoload
(defun org-wild-notifier-schedule-at-point (minutes)
  "Schedule a notification for the org heading at point.
MINUTES is the number of minutes from now to trigger the notification."
  (interactive "nMinutes until notification: ")
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in an org-mode buffer"))
  (let ((title (org-get-heading t t t t)))
    (unless title
      (user-error "Not on an org heading"))
    (run-at-time (* minutes 60) nil
                 (lambda (msg)
                   (org-wild-notifier--notify msg))
                 (format "%s (scheduled reminder)" title))
    (message "Notification scheduled for \"%s\" in %d minute%s"
             title minutes (if (= minutes 1) "" "s"))))

;;;###autoload
(defun org-wild-notifier-schedule-at-point-at-time (time)
  "Schedule a notification for the org heading at point at TIME.
TIME is a string in HH:MM format or any format accepted by `org-read-date'."
  (interactive
   (list (org-read-date nil nil nil "Notification time: ")))
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in an org-mode buffer"))
  (let* ((title (org-get-heading t t t t))
         (target-time (org-read-date nil t time)))
    (unless title
      (user-error "Not on an org heading"))
    (when (time-less-p target-time (current-time))
      (user-error "Cannot schedule notification in the past"))
    (run-at-time target-time nil
                 (lambda (msg)
                   (org-wild-notifier--notify msg))
                 (format "%s (scheduled reminder)" title))
    (message "Notification scheduled for \"%s\" at %s"
             title (format-time-string "%Y-%m-%d %H:%M" target-time))))

(defun org-wild-notifier--get-notifications-for-event (event)
  "Get all notifications for EVENT.
Returns a list of notification plists, each containing:
  :notify-at - Emacs time when notification should fire
  :event-time - Emacs time of the event (nil for absolute/day-wide)
  :event-time-string - Original timestamp string (nil for absolute/day-wide)
  :timestamp-type - deadline, scheduled, or timestamp (nil for abs/day-wide)
  :minutes-before - Minutes before event (nil for absolute/day-wide)
  :type - Symbol: relative, absolute, or day-wide
  :title - Event title string
  :marker - Org marker for navigation/queries
  :event - The full event alist"
  (let ((notifications nil)
        (marker (cdr (assoc 'marker event)))
        (title (cdr (assoc 'title event)))
        (times (cadr (assoc 'times event)))
        (intervals (cdr (assoc 'intervals event)))
        (notify-at-times (cdr (assoc 'notify-at event))))
    ;; Check relative notifications (intervals before event times)
    (dolist (time-info times)
      (let ((timestamp-string (plist-get time-info :timestamp-string))
            (event-time (plist-get time-info :time))
            (timestamp-type (plist-get time-info :timestamp-type)))
        (when (org-wild-notifier--has-timestamp timestamp-string)
          (dolist (interval intervals)
            (let ((notify-time (time-subtract event-time (seconds-to-time (* 60 interval)))))
              (push (list :notify-at notify-time
                          :event-time event-time
                          :event-time-string timestamp-string
                          :timestamp-type timestamp-type
                          :minutes-before interval
                          :type 'relative
                          :title title
                          :marker marker
                          :event event)
                    notifications))))))
    ;; Check absolute notifications (notify-at times)
    (dolist (notify-time notify-at-times)
      (push (list :notify-at notify-time
                  :event-time nil
                  :event-time-string nil
                  :timestamp-type nil
                  :minutes-before nil
                  :type 'absolute
                  :title title
                  :marker marker
                  :event event)
            notifications))
    ;; Check day-wide notifications (overdue or day-wide events at configured times)
    (when (org-wild-notifier-display-as-day-wide-event event)
      (dolist (notify-time (org-wild-notifier--day-wide-alert-times-for-today))
        (push (list :notify-at notify-time
                    :event-time nil
                    :event-time-string nil
                    :timestamp-type nil
                    :minutes-before nil
                    :type 'day-wide
                    :title title
                    :marker marker
                    :event event)
              notifications)))
    (nreverse notifications)))

(defun org-wild-notifier--gather-events-sync ()
  "Synchronously gather events from agenda files.
Returns a list of event alists suitable for notification processing."
  (let ((org-agenda-use-time-grid nil)
        (org-agenda-compact-blocks t)
        (events nil))
    (save-window-excursion
      (org-agenda-list 2 (org-read-date nil nil "today"))
      (with-current-buffer org-agenda-buffer-name
        (setq events
              (->> (org-split-string (buffer-string) "\n")
                   (--map (plist-get
                           (org-fix-agenda-info (text-properties-at 0 it))
                           'org-marker))
                   (-non-nil)
                   (org-wild-notifier--apply-whitelist)
                   (org-wild-notifier--apply-blacklist)
                   (-map 'org-wild-notifier--gather-info))))
      (kill-buffer org-agenda-buffer-name))
    events))

;;;###autoload
(defun org-wild-notifier-get-notifications-for-marker (marker)
  "Get all notifications for the org entry at MARKER.
Returns a list of notification plists, each containing:
  :notify-at - Emacs time when notification should fire
  :event-time - Emacs time of the event (nil for absolute/day-wide)
  :event-time-string - Original timestamp string (nil for absolute/day-wide)
  :timestamp-type - deadline, scheduled, or timestamp (nil for abs/day-wide)
  :minutes-before - Minutes before event (nil for absolute/day-wide)
  :type - Symbol: relative, absolute, or day-wide
  :title - Event title string
  :marker - Org marker for navigation/queries
  :event - The full event alist"
  (let ((event (org-wild-notifier--gather-info marker)))
    (org-wild-notifier--get-notifications-for-event event)))

;;;###autoload
(defun org-wild-notifier-get-upcoming-notifications (&optional now)
  "Get all notifications for upcoming events.
If NOW is provided, use it as the current time for calculations.
This allows querying notifications as if it were a different time.
Returns a list of notification plists, each containing:
  :notify-at - Emacs time when notification should fire
  :event-time - Emacs time of the event (nil for absolute/day-wide)
  :event-time-string - Original timestamp string (nil for absolute/day-wide)
  :timestamp-type - deadline, scheduled, or timestamp (nil for abs/day-wide)
  :minutes-before - Minutes before event (nil for absolute/day-wide)
  :type - Symbol: relative, absolute, or day-wide
  :title - Event title string
  :marker - Org marker for navigation/queries
  :event - The full event alist"
  (org-wild-notifier--get-upcoming-notifications-1
   (if now (lambda () now) (symbol-function 'current-time))))

(defun org-wild-notifier--get-upcoming-notifications-1 (time-fn)
  "Gather upcoming notifications using TIME-FN as `current-time'."
  (cl-letf (((symbol-function 'current-time) time-fn))
    (let* ((events (org-wild-notifier--gather-events-sync))
           (all-notifications nil))
      (dolist (event events)
        (let ((event-notifications
               (org-wild-notifier--get-notifications-for-event event)))
          (setq all-notifications (append all-notifications event-notifications))))
      all-notifications)))

(provide 'org-wild-notifier)

;;; org-wild-notifier.el ends here
