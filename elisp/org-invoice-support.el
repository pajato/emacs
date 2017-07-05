;; Support for handling invoices and time logging and planning.

(define-key org-mode-map [f13] 'org-table-recalculate-buffer-tables)
(define-key org-mode-map [S-f10] 'org-table-recalculate-buffer-tables)

(defvar table-names '("Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday" "Sunday"))
(defvar billing-alist "The key is the billing code, e.g. swi-pe, and the value is a category-alist.")
(defvar iteration-index nil)

(defun invoice-process-daily-data ()
  "Process the daily data to populate the billing-alist object.
Return the number of hours processed for each day, from Monday to
Sunday."
  (cond
   ((or (null iteration-index) (< iteration-index 1) (>= iteration-index 7))
    (invoice-build-summary-tables)
    (setq iteration-index 1))
   (t (setq iteration-index (1+ iteration-index))))
  (let ((category-alist (nth 1 (assoc "all" billing-alist)))
        result)
    (setq sums (mapcar 'invoice-get-sums table-names))
    (nth (1- iteration-index) sums)))

(defun invoice-get-sums (name)
  "Return the total amount of time entered for the day named NAME."
  (let ((categories category-alist)
        (sum 0)
        entry minutes result tally)
    (while categories
      (setq entry (car categories)
            categories (cdr categories)
            tally (assoc name (nth 1 entry))
            minutes (if tally (nth 1 tally) 0)
            sum (+ sum minutes)))
    (/ sum 60.0)))

(defun invoice-build-summary-tables ()
  "Populate CATEGORY-ALIST with the content of the tables named in TABLE-NAMES."
  (let ((days table-names)
        base cat cat-entry day-entry beg beg billable bill-to count day list tally)
    (setq billing-alist nil)
    (while days
      (setq day (car days)
            days (cdr days)
            data (org-table-get-remote-range day "@4$CAT..@>$BT")
            count (/ (length data) 6))
      ;; Populate the category alist for the given day.
      (dotimes (index count count)
        (setq base (* index 6)
              cat (nth base data)
              beg (invoice-time-string-to-minutes (nth (+ 3 base) data))
              end (invoice-time-string-to-minutes (nth (+ 4 base) data))
              bill-to (nth (+ 5 base) data)
              delta (- end beg))
        ;; Insure that the category and bill-to variables have useful values.
        (if (string= cat "")
            (setq cat "uncategorized"))
        (if (string= bill-to "")
            (setq bill-to "unbillable"))

        ;; Add the current item into the category alist for the associated billing target.
        (cond
         ;; Determine if a new billing list should be created.
         ((null billing-alist)
          ;; It should.  Create it with a single, zeroed entry for the
          ;; current bill-to target and the "all" aggregator.
          (setq billing-alist (list (list bill-to (list (list cat (list (list day 0)))))
                                    (list "all" (list (list cat (list (list day 0))))))))
         ;; Determine if a new bill-to target is being used in the current billing-alist object.
         ((null (assoc bill-to billing-alist))
          ;; The bill-to target is a new one.  Add a zeroed entry to the billing-alist object.
          (nconc billing-alist (list (list bill-to (list (list cat (list (list day 0))))))))
         ;; Otherwise the billing-alist object is ready to use.
         (t nil))

        ;; Populate the category alist for a given bill-to target.
        ;; The accessed category-alist will never be null.
        (invoice-add-entry bill-to cat day delta)
        (invoice-add-entry "all" cat day delta))))

  ;; Now generate a summary table for each billing target, including the "all" aggregator.
  (mapcar 'invoice-generate-summary billing-alist))

(defun invoice-add-entry (target cat day delta)
  "Add a time entry (category CAT for day DAY with duration
DELTA) to the billing target TARGET."
  (let ((category-alist (nth 1 (assoc target billing-alist))))
    (cond
     ;; Determine if a new category has been encountered in the existing category-alist object.
     ((null (assoc cat category-alist))
      ;; The category is new.  Create an entry in the category-alist object for it.
      (nconc category-alist (list (list cat (list (list day delta))))))
     ;; Determine if there is not a day entry for the given day and category. 
     ((null (assoc day (nth 1 (assoc cat category-alist))))
      ;; There is not such an entry.  Add one.
      (let ((entry (nth 1 (assoc cat category-alist))))
        (nconc entry (list (list day delta)))))
     ;; Otherwise, add an entry to the appropriate, existing category item.
     (t
      (let ((entry (assoc day (nth 1 (assoc cat category-alist)))))
        (setcar (cdr entry) (+ delta (nth 1 entry))))))))

(defun invoice-generate-summary (entry)
  ;; Insert the include parameter for this BILL-TO target.
  (let ((category-alist (nth 1 (assoc (car entry) billing-alist)))
        (summary-filename (format "summary-%s.org" (car entry)))
        sums)
    (save-excursion
      ;; switch to buffer named "summary-${BILL-TO}.org", creating the
      ;; buffer as necessary, erase the content, and persist the file.
      (find-file summary-filename)
      (erase-buffer)
      (insert "|-|-|\n")
      (insert "||Category|")
      (mapcar (lambda (name) (insert (format "%s|" (substring name 0 3)))) table-names)
      (insert "Total|\n")
      (insert "|-|-|\n")
      (insert "| ! | CAT |\n")
      (insert "| / | <15> | <5> | <5> | <5> | <5> | <5> | <5> | <5> | <6> |\n")
      (mapcar 'invoice-emit-category-row category-alist)
      (insert "|-|-|\n")
      (insert "|/|Totals|")
      (setq sums (mapcar 'invoice-emit-totals-row table-names))
      (insert (format "%.2f|\n" (apply '+ sums)))
      (insert "|-|-|\n")
      (goto-char (+ 1 (point-min)))
      (org-ctrl-c-ctrl-c)
      (save-buffer))
    (switch-to-buffer (current-buffer))
    sums))

(defun invoice-emit-totals-row (name)
  "Emit the table row for the totals for all categories for a given NAME."
  (let ((categories category-alist)
        (sum 0)
        entry minutes result tally)
    (while categories
      (setq entry (car categories)
            categories (cdr categories)
            tally (assoc name (nth 1 entry))
            minutes (if tally (nth 1 tally) 0)
            sum (+ sum minutes)))
    (setq result (/ sum 60.0))
    (insert (format "%.2f|" result))
    result))

(defun invoice-emit-category-row (entry)
  "Process a category by emitting the table row for the associated category."
  (let ((names table-names)
        (sum 0)
        day-entry total)
    (insert (format "||%s|" (nth 0 entry)))
    (while names
      (setq name (car names)
            names (cdr names)
            day-entry (assoc name (nth 1 entry))
            total (if day-entry (nth 1 day-entry) 0)
            sum (+ sum total))
      (insert (format "%.2f|" (/ total 60.0))))
    (insert (format "%.2f|\n" (/ sum 60.0)))))

(defun invoice-time-string-to-minutes (s)
  "Convert a string HH:MM to a number of minutes."
  (cond
   ((and (stringp s)
         (string-match "\\([0-9]+\\):\\([0-9]+\\)" s))
    (let ((hour (string-to-number (match-string 1 s)))
          (min (string-to-number (match-string 2 s))))
      (+ (* hour 60) min)))
   ((and (stringp s)
         (string-match "\\([0-9]+\\)" s))
    (let ((min (string-to-number (match-string 1 s))))
      min))
   ((stringp s) (string-to-number s))
   (t 0)))

(defun invoice-time-minutes-to-hm-string (format mins)
  "Convert a number of minutes to a time string."
  (cond ((and (string= format "FLT") (>= mins 60)) (format "%.2f" (/ mins 60.0)))
        ((and (string= format "HM") (>= mins 60)) (format-seconds "%h:%2m" (* mins 60)))
        ((and (string= format "FLT") (>= mins 1)) (format "%.2f" (/ mins 60.0)))
        ((and (string= format "HM") (>= mins 1)) (format-seconds "0.%.2m" (* mins 60)))
        (t "0.00")))

(provide 'org-invoice-support)
