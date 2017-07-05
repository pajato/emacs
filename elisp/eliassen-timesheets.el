;;; Emacs lisp package for generating a Pencom Systems Incorporated
;;; time sheet using data generated from Jonathan Stigelman's very
;;; slick `Timecard' Emacs lisp package.

(require 'timecard-mode)

(defvar ts-days '("Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday" "Sunday"))

(defvar ts-months '(("01" "January")
		    ("02" "February")
		    ("03" "March")
		    ("04" "April")
		    ("05" "May")
		    ("06" "June")
		    ("07" "July")
		    ("08" "August")
		    ("09" "September")
		    ("10" "October")
		    ("11" "November")
		    ("12" "December")))

;;; Utility routines

(defun ts-convert-timestring (time-string)
  ""
  (let ((trailer (if (string= (substring time-string 2) ":30") ".5" "")))
    (cond ((string= "     " time-string)
	   "-")
	  ((char-equal ?0 (aref time-string 0))
	   (concat (substring time-string 1 2) trailer))
	  (t (concat (substring time-string 0 2) trailer)))))

;;; Eliassen Time-Sheet generation routine.

(defun ts-make-timesheet ()
  "Generate a .html file to be used to print a time sheet for submission to
Eliassen."
  (interactive)
  
;;; the current buffer is the weekly summary.
  
  (let (weekly-total weekly-data week-ending)
    (if (re-search-forward (format "^%s \\[ \\(%s\\) \\]"
				   timecard-grandtotal-str 
				   timecard-time-regex) (point-max) t)
	(progn
	  (setq weekly-total
		(buffer-substring (match-beginning 1) (match-end 1))))
      (error "The current buffer does not contain a Ts summary."))
    (let ((days ts-days))
      (while days
	(if (re-search-forward (format "^\\(%s\\)  \\(     \\|%s\\)"
				       timecard-date-regex
				       timecard-time-regex) (point-max) t)
	    (setq weekly-data 
		  (append weekly-data
			  (list (cons (car days)
				      (list (buffer-substring
					     (match-beginning 1)
					     (match-end 1))
					    (buffer-substring
					     (match-beginning 2)
					     (match-end 2))))))
		  days (cdr days))
	  (error "The daily list is ill-formatted."))))
    (set-buffer (find-file-noselect
		 (concat "~/eliassen/pmr-"
			 (substring (current-time-string) -2) "."
			 (substring (car (cdr (nth 6 weekly-data))) 0 2) "."
			 (substring (car (cdr (nth 6 weekly-data))) 3) ".html")))
    (insert-file-contents "~/eliassen/ts-template" nil nil nil t)
    (goto-char (point-min))
    (setq week-ending (concat (car (cdr (nth 6 weekly-data))) "/"
			      (substring (current-time-string) -4)))

    ;; Generate the header information.
    (if (search-forward "{ts-date}" (point-max) t)
	(replace-match week-ending)
      (error "T/S date placeholder not found."))

    ;; Generate the daily information.
    (let ((list weekly-data)
	  day entry)

      ;; Generate the dates.
      (while list
	(setq entry (car list)
	      day (car (car list))
	      list (cdr list))
	(if (search-forward (format "{%s-date}" day) (point-max) t)
	    (replace-match (nth 1 entry))
	  (error (format "%s's date placeholder not found." day))))
      (setq list weekly-data)

      ;; Generate the hours.
      (while list
	(setq entry (car list)
	      day (car (car list))
	      list (cdr list))
	(if (search-forward (format "{%s-hours}" day) (point-max) t)
	    (replace-match (nth 2 entry))
	  (error (format "%s's hours placeholder not found." day)))))

    ;; Generate the total hours worked.
    (if (search-forward "{total-hours}" (point-max) t)
	(replace-match weekly-total)
      (error "Total hours placeholder not found."))

    ;; Generate the week ending data.
    (let ((month (nth 1 (assoc (substring week-ending 0 2) ts-months)))
	  (date (substring week-ending 3 5))
	  (year (substring week-ending -4)))
      (if (search-forward "{week-ending}" (point-max) t)
	  (replace-match (format "%s %s, %s" month date year))
	(error "Week ending placeholder not found.")))
    (save-buffer)))

(provide 'eliassen-timesheets)