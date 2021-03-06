;; timecard-mode.el (Exp 1.38) -- timecard utility for GNU Emacs
;;
;;{{{ Copyright (C) 1993, 1995 Jonathan Stigelman <Stig@hackvan.com>

;; Copyright (C) 1993, 1995 Jonathan Stigelman <Stig@hackvan.com>
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;;

;;}}}
;;{{{ NOTICE: As-is...  If it screws up, then you lose...not me.

;; NOTICE:
;;
;;   I wrote this mode for *ME* and it seems to do what I want it to.  I
;;   happen to trust it not to screw up my timecard too, but that doesn't
;;   mean that you should...pay attention to what it's doing until you're
;;   sure that it works on your version of emacs with whatever other
;;   packages you have loaded.  If timecard-mode, by some cosmic quirk,
;;   goofs up, then you have an UNDO key....
;;
;;   I post it in order to share it with anyone who might find it to be
;;   useful, but I'm not particularly interested in adapting it to meet the
;;   needs of anyone else.  I would be delighted to receive fixes for
;;   anything that's broken...just so long as I'm not expected to spend any
;;   time on them.
;;  
;;   That this package is not a high priority for me should be obvious since
;;   I wrote it in mid '93 and haven't really done anything to it since then.
;;   Some of the original design decisions lacked foresight.
;;   Nowadays, I write much better elisp but I don't feel at all compelled
;;   to work on this.  You're welcome to, however...
;;

;;}}}
;;{{{ documentation

;; INSTALLATION:
;; 
;;   (global-set-key "\C-x\C-t"     (make-sparse-keymap)) ; if you also use v18
;;   (global-set-key "\C-x\C-t\C-i" 'timecard-checkin)
;;   (global-set-key "\C-x\C-t\C-o" 'timecard-checkout)
;;   (global-set-key "\C-x\C-t\C-t" 'timecard-find-timecard)
;;
;;   (setq timecard-define-menus t)    ; only if you want menus
;;
;;   (autoload 'timecard-find-timecard "timecard-mode" nil t)
;;   (autoload 'timecard-checkin       "timecard-mode" nil t)
;;   (autoload 'timecard-checkout      "timecard-mode" nil t)
;;   (autoload 'timecard-mode          "timecard-mode" nil t)
;; 
;; TIPS:
;;
;; * Just checkin or checkout.  The timecard file will be setup
;;   automatically.  You might want to define the environment variable
;;   TIMECARD if you don't like ~/notes/log as a name for it.
;;
;; KNOWN SHORTCOMINGS -- TELL ME IF YOU FIX 'EM:
;;
;; * The companion shell script is not super portable.  /bin/sh on
;;   sparcstations is a ksh derivative, I think, and the script uses the 
;;   extra features of ksh.  Now, however, it's use is entirely optional.
;;   If it happens to work for you, then put it in a window manager menu.
;;   Otherwise, don't ask me to fix it.  (But I might install patches.)
;;
;; * Paragraph filling is a kludge because forward-paragraph is broken.
;;
;; BUGS/FEEDBACK:
;;
;;        M-x timecard-submit-feedback RET
;;
;; WHERE TO GET THE "LATEST" VERSIONS OF TIMECARD.EL VIA ANONYMOUS FTP:
;;
;;   ftp.netcom.com:/pub/st/stig/src/elisp/{Beta,Release}/timecard-mode.el.gz
;;
;; LCD Archive Entry:
;; timecard-mode|Jonathan Stigelman|Stig@hackvan.com|
;; Timecard maintenance mode for Emacs 18 & 19.  Does accounting.|
;; 1995/01/24 21:50:35|Exp 1.38|~/modes/timecard-mode.el.Z|

;;}}}
;;{{{ history

;; timecard-mode.el,v
;; Revision 1.38  1995/01/24 21:50:35  stig
;; *** empty log message ***
;;
;; Revision 1.37  1995/01/22 08:51:36  stig
;; doc fixup
;;
;; Revision 1.36  1995/01/20 17:19:29  stig
;; now set comment-column and comment-start so that M-; will indent
;; appropriately and insert a paragraph break into timecard annotations.
;;
;; Revision 1.35  1995/01/20 13:23:32  stig
;; - edited release notes and timecard-submit-feedback to reflect my current
;;   feelings about maintaining this code...
;;
;; - If you don't use rates for anything (I don't...they're too limited),
;;   then weekly summaries are more to the point.
;;
;; Revision 1.34  1995/01/20  12:37:45  stig
;; Minor bug fixes...
;; - fixed up some macro ordering dependencies for proper byte-compilation.
;; - changed (end-of-buffer) to (goto-char (point-max)) in a few places.
;; - no longer stomps on the value of require-final-newline (I'm so ashamed!).
;;
;; Revision 1.33  1993/09/04  08:21:10  stig
;; minor fix to the last change
;;
;; Revision 1.32  1993/09/04  08:11:57  stig
;; now permit negative numeric argument to checkin and checkout.
;; If it's 12:00 and you type ^U -10 ^C^I, then the checkin time is 11:50.
;;
;; Revision 1.31  1993/09/01  17:49:07  stig
;; fixed bug in timecard-date-string
;;
;; Revision 1.30  1993/08/31  20:56:47  stig
;; fixed minor bug in timecard-checkout
;;
;; Revision 1.29  1993/08/23  22:09:36  stig
;; applied patch from Peter Brown <brownp@cs.unc.edu> to improve FSF19 menus.
;;
;; Revision 1.28  1993/08/23  22:05:22  stig
;; improved behavior of continue-current-entry option for checkin.
;;
;; Revision 1.27  1993/08/20  13:08:07  stig
;; Oops!  I just released this to the net with a broken regular expression...
;;
;; Revision 1.26  1993/08/20  12:06:12  stig
;; Minor tuning to "continue anywhere" feature of checkin.
;;
;; Checkin now automatically inserts the day of the week, which get highlighted
;; as borders.  Timecard-insert-border now inserts two lines of dashes.
;;
;; Revision 1.25  1993/08/19  11:02:51  stig
;; added folds and reorganized file
;;
;; ^U ^U M-x timecard-checkin now continues the current entry instead of
;; continuing the last entry in the file.
;;
;; ^U number M-x timecard-checkout now checks out the current line if the
;; current line matches timecard-checked-in-regex.
;;
;; Revision 1.24  1993/08/16  04:32:25  stig
;; fixed bugs in hilit patterns that appeared because of the multinational
;; stuff
;;
;; Revision 1.23  1993/08/16  01:46:44  stig
;; merged in support for alternate date formats and foreign language keywords.
;;   Patch provided by Markus Ast <w9am@sun-1.intes-stuttgart.de>,
;;   pain & sufferring by me.
;;
;; There's a start at support for editing entries in the middle of the file,
;; but it's not yet functional.
;;
;; Operation of tabs improved.
;;
;; Revision 1.22  1993/08/12  19:52:06  stig
;; added variable timecard-define-menus and code to define menus in Lucid and
;; FSF19.  Lucid menus by Tim Bradshaw <tim.bradshaw@mid-heidelberg.de>.
;;
;; Revision 1.21  1993/08/11  22:46:41  stig
;; added timecard-fill-paragraph and it seems to do the right thing.
;;     paragraph-start and paragraph-separate are simply insufficient for
;;     the needs of timecard-mode.  Pity.
;;
;; added timecard-newline-and-indent which indents new lines more intelligently.
;;
;; Revision 1.20  1993/07/24  14:16:21  stig
;; added LCD entry and timecard-submit-feedback
;;
;; worked on paragraph filling, but it's still not quite right.
;;
;; Revision 1.19  1993/07/23  20:27:34  stig
;; reshuffled macro definitions so that compiling w/ version 18 emacs wouldn't
;; cause version 19 to puke on the compiled lisp.
;;
;; Revision 1.18  1993/07/22  15:20:58  stig
;; now works w/ emacs 18 -- converted rates to percentages instead of floats
;;

;;}}}

;;{{{ User Options

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User Options:

;;{{{ user options

(defvar timecard-file (or (getenv "TIMECARD") (getenv "WLOG")
                          (expand-file-name "~/notes/log"))
  "* Name of the timecard-file")

(defvar timecard-checkin-command nil
  "* The command sent to /bin/sh after a checkin or NIL.
Example: (format \"(xset -display %s led on; touch %s.lock)&\"
		 (getenv \"DISPLAY\") timecard-file)")

(defvar timecard-checkout-command nil
  "* The command sent to /bin/sh after a checkout or NIL.
Example: (format \"(xset -display %s led off; rm %s.lock)&\"
		 (getenv \"DISPLAY\") timecard-file)")

(defvar timecard-no-continuations nil
  "* don't use timecard-continuation-str as the checkin time
for contiguous time blocks")

;;}}}
;;{{{ user configurable pseudo-constants

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Pseudo-constants must be set BEFORE you load timecard-mode.el...

(defvar timecard-define-menus nil
  "should timecard-mode define global and local pop-up menus?")

(defvar timecard-european-date nil
  "Non-nil means use european style dates,i.e. Day/Month instead of Month/Day")

(defvar timecard-numeric-date  t
  "Non-nil means use numeric dates, e.g. \"12/08\" instead of \"12-Dec\"")

;;}}}
;;{{{ more obscure variables

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Variables that the user will generally not bother to modify:

(defvar timecard-command-buffer " *timecard-cmd*"
  "If this has a leading space then it's normally invisible to the user.")

(defvar timecard-rate-alist nil
  "List of rates (partial billings) to apply to different accounts.
This number is an integer percentage.

The severely limited utility of `rates' is probably one of the biggest
failings of timecard-mode.  Sorry.")
(make-variable-buffer-local 'timecard-rate-alist)

(defvar timecard-mode-abbrev-table nil
  "Abbrev table in use in timecard-mode buffers.")

(define-abbrev-table 'timecard-mode-abbrev-table ())

;;}}}

;;{{{ timecard-mode-map

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Local keymap:

(defvar timecard-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-i" 'timecard-checkin)
    (define-key map "\C-c\C-o" 'timecard-checkout)
    (define-key map "\C-c\C-b" 'timecard-insert-border)
    (define-key map "\C-c\C-d" 'timecard-sum-day)
    (define-key map "\C-c\C-w" 'timecard-sum-week)
    (define-key map "\C-j" 'newline)
    (define-key map "\C-m" 'timecard-newline-and-indent)
    (define-key map "\M-q" 'timecard-fill-paragraph)
    (define-key map "\t"   'tab-to-tab-stop) ; was: indent-for-tab-command
    map))

;;}}}
;;{{{ constants & pseudo-constants

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constants and more pseudo-constants -- Don't push that button!!!
;;
;; Pseudo-constants must be set BEFORE you load timecard-mode.el...
;;
;; Constants cannot be set before you load timecard-mode.el, and probably
;; shouldn't be anywhere except in timecard-mode.el itself.
;;

;; Some standard strings to enable foreign language customizations
(defvar timecard-rate-str       "RATE" "Keyword to recognize rate-entries.")
(defvar timecard-grandtotal-str	"TOTAL" "String for \"TOTAL\".")
(defvar timecard-billed-str	"- billed -" "String for \"- billed -\"")
(defvar timecard-unbilled-str	"- unbilled -" "String for \"- unbilled -\"")
(defvar timecard-subtotals-str	"- total hrs -" "String for \"- total hrs -\"")
(defvar timecard-date/hours-str "DATE   HOURS   " "String for \"DATE   HOURS   \"")

(defconst timecard-rate-regex (concat
  "^" timecard-rate-str ":\\s *\\(\\w+\\)\\s +\\([0-9]+\\)%")
  "Regular expression for a timecard RATE entry")

(defconst timecard-date-regex
  (if timecard-european-date
     (if timecard-numeric-date "[0123][0-9]/[01][0-9]"
			       "[0123][0-9]-[A-Z][a-z][a-z]")
     (if timecard-numeric-date "[01][0-9]/[0123][0-9]"
			       "[A-Z][a-z][a-z]-[01][0-9]"))
"Regular expression for a timecard date entry, derived from user settable
variables 'timecard-european-date' and timecard-numeric-date'.")

(defconst timecard-time-regex "[0-9][0-9]:[0-5][0-9]"
  "Regular expression for a timecard time entry in form HH:MM")

(defconst timecard-continuation-str "    >"
  "four spaces and a right angle-bracket.  See timecard-no-continuations.")

(defconst timecard-checked-in-regex
  (format "^%s:\t\\(%s\\|%s\\) -\\s *$"
	  timecard-date-regex
	  timecard-time-regex
	  timecard-continuation-str)
  "Regular expression for timecard-check-in")

(defconst timecard-account-regex "\\**\\([-._/A-Za-z0-9]*\\)?\\**"
  "Regular expression for a timecard account entry, e.g. *account* ...")

(defconst timecard-entry-regex
  (format "^\\(%s\\):\t\\(%s\\|%s\\) - \\(%s\\)\t%s"
	  timecard-date-regex
	  timecard-time-regex timecard-continuation-str
	  timecard-time-regex
	  timecard-account-regex)
"Matches a timecard entry...so long as the tabs haven't been goofed up.
subexpressions: (1) date, (2) timein, (3) timeout, (4) account")

(defconst timecard-summary-regex
  (format 
   "^\\(%s\\):: \\[ \\(%s\\) ]"
   timecard-date-regex
   timecard-time-regex)
  "Regular expression for timecard-summary entry.")

(defconst timecard-border-regex "^------------.*"
  "Regular expression to match a timecard-border line...this separates weeks.")

;;}}}

;;}}}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Code:

;;{{{ timecard-mode

;;;###autoload
(defun timecard-mode ()
  "Major mode for editing timecards created with the timecard shell script.
Formatting is important, and this is the format:

        ------------ Joe McJoe <joe@joe.com>        -*- timecard -*-
        RATE: lunch 0%
        
        07/12:  08:00 - 09:00   *meeting* Bob made the coolest paper
                                airplane during Hal's presentation.
        07/12:      > - 12:00   *proj1* Chug Chug
        07/12:  12:00 - 13:00   *lunch* The Black Sun
        07/12:      > - 17:00   *proj2* Grind Grind
        07/12:: [ 09:00 ] = 540 min = 240 + 300
        
        
        TOTAL [ 08:00 ]
        
        DATE   HOURS   meeting  proj1  lunch  proj2  
        07/12  08:00   01:00    03:00  01:00  04:00  
        
        - billed -     01:00    03:00         04:00  
        - unbilled -                   01:00         
        - total hrs -  01:00    03:00  01:00  04:00  


Commands worthy of note:
  \\[timecard-checkin]  checks you in and runs timecard-checkin-command
  \\[timecard-checkout] checks you out and runs timecard-checkout-command
  \\[timecard-insert-border]    inserts a border that sets weeks apart.

  \\[timecard-sum-day]  counts up your hours for the day and inserts a summary
                line at the end of the day.  Days are separated by blank lines.
  \\[timecard-sum-week] summarizes each day of the week, then creates a report
                for the whole week.  Weeks are delimited by lines of dashes
                like those created by \\[timecard-insert-border].

IMPORTANT:  The functions that tabulate hours are particular about the
formatting of the timecard entry lines.  If you screw up the formatting, then
you screw up the final result as well.

Timecard-mode has its own abbrev table and keymap.

Turning on TIMECARD mode calls the value of the variable `timecard-mode-hook'
with no args, if that value is non-nil."
  (interactive)
  (or (eq major-mode 'timecard-mode)
      (timecard-mode-setup)))

(defun timecard-mode-setup ()
  (kill-all-local-variables)

  (make-local-variable 'paragraph-separate)
  (make-local-variable 'paragraph-start)

  (use-local-map timecard-mode-map)
  (set (make-local-variable 'require-final-newline) nil)
  (setq major-mode 'timecard-mode
        mode-name "TIMECARD"
	comment-column 24
	comment-start "* "
        paragraph-separate (concat "^[ \t\f]*$") ;; "\\|" paragraph-start)
        paragraph-start (mapconcat
			 'identity
			 (list timecard-rate-regex timecard-border-regex
			       timecard-entry-regex timecard-checked-in-regex
			       timecard-summary-regex "^\\s *\\*+")
			 "\\|")
        fill-column 79
	fill-prefix "\t\t\t"
        local-abbrev-table timecard-mode-abbrev-table)

  (set (make-local-variable 'tab-stop-list)
       '(8 14 16 24 32 40 48 56 64 72))

  (cond ((string-match "^19" emacs-version)
	 (set (make-local-variable 'adaptive-fill-mode) nil))
	(t 
	 ;; (set (make-local-variable 'paragraph-ignore-fill-prefix) t)
	 (set (make-local-variable 'indent-line-function)
	      'timecard-indent-line)))

  (turn-on-auto-fill)

  (goto-char (point-max))
  (if (not (bolp)) (progn (previous-line 1) (end-of-line)))
  (and (featurep 'hilit19)
       (hilit-set-mode-patterns
        'timecard-mode
	(list
	 ;; if it doesn't highlight, then it's not gonna get totalled either

	 (list (concat timecard-entry-regex "\\|^\t\t\t\\*+") nil 'purple)
	 (list timecard-rate-regex  nil 'red-bold-italic)
         '("-----.*"  nil firebrick-bold-italic)
         (list timecard-summary-regex nil 'ForestGreen-bold)
	 
	 ;; GREEN highlights
         (list (format "^%s +%s" timecard-date-regex timecard-time-regex)
	       nil 'ForestGreen-bold)
         (list (concat "^" timecard-grandtotal-str  ".*") nil 'ForestGreen-bold)
         (list (format "^%s.*" timecard-billed-str) nil 'ForestGreen-bold)

	 ;; PURPLE
         (list (format "^%s" timecard-date/hours-str)
	       (concat "^" timecard-subtotals-str ".*$") 'purple)
	 )))

  (run-hooks 'timecard-mode-hook))

;;}}}
;;{{{ find-timecard, goto-eotc

;;;###autoload
(defun timecard-find-timecard (&optional other-win)
  "Find the timecard-file in the other window unless we're already there."
  (interactive "P") 
  (or (string= buffer-file-name timecard-file)
      (funcall (if other-win 'find-file-other-window 'find-file)
               timecard-file))
  (cond ((and (bobp) (eobp))
         (insert
          (format "%-60s -*- timecard -*-"
                  (concat "------------ " (user-full-name) " <"
                          (user-login-name) "@" (system-name) "> ")))))
  (timecard-mode)
  (goto-char (point-max)))

;;;###autoload
(defun timecard-goto-eotc (&optional strip-day-sum)
  "Go to the end of the buffer and remove all the whitespace"
  (interactive)
  (or (eq major-mode 'timecard-mode)
      (timecard-find-timecard t))
  (goto-char (point-max))
  (delete-blank-lines)                  ; once leaves one line
  (delete-blank-lines)                  ; twice kills 'em all
  (forward-char -1)
  (save-restriction
    (timecard-narrow-to-week)
    (cond ((re-search-backward (concat "\n\n" timecard-grandtotal-str " \\[")
                               nil t)
           (delete-region (point) (point-max))
           (delete-blank-lines) (delete-blank-lines))
          (t (goto-char (point-max))))
    (and strip-day-sum
         (save-excursion
           (forward-line -1)
           (if (looking-at timecard-summary-regex)
               (kill-line 1)))))
  (if (bolp) (forward-char -1)))

;;}}}

;;{{{ submit-feedback

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; To submit bug reports...

;; doesn't work w/ v18 (eval-when-compile (require 'reporter)) 

(defun timecard-submit-feedback ()
  "Send email to the author of timecard-mode"
  (interactive)
  (require 'reporter)
  (and (y-or-n-p "Do you really want to submit a report on timecard-mode? ")
       (reporter-submit-bug-report
        "Jonathan Stigelman <Stig@hackvan.com>"
        "timecard-mode.el (Exp 1.38)"
        nil ; varlist
	'((lambda ()
	    (mapcar (lambda (x) (message x) (sit-for 2))
		    '("Paying for service to this code is tax deductible..."
		      "Lower your tax bracket by donating a large sum..."
		      "Buy one, get one free!!!"
		      "That's a patch that you're about to mail to me, right?"
		      "It isn't?  Awww, what the hell?  I *might* not be busy."
		      "Allow 4 to 6 weeks for delivery..."
		      "(Offer void where prohibited by law.)"
		      ))
	    )) ; pre-hooks
	nil ; post-hooks
        "Hey Stig, I agree that timecard-mode is warped:\n")))

;;}}}
;;{{{ fill-paragraph, newline-and-indent, indent-line

(defun timecard-fill-paragraph ()
  "In timecard-mode, fill paragraph at or after point."
  (interactive)
  (save-excursion 
    (let (beg end adaptive-fill-mode)
      (let (fill-prefix)      
        (forward-paragraph)
        (or (bolp) (newline 1))

        (setq end (point-marker))

        (backward-paragraph)
        (re-search-forward paragraph-start nil t)
        (setq beg (point))
        (insert (make-string (current-column) ?x))
        )
      (fill-region-as-paragraph beg end nil)
      (set-marker end nil)
      (goto-char beg)
      (delete-char (current-column))
      )))

(defun timecard-newline-and-indent ()
  "In timecard-mode, insert a newline and indent the new line appropriately"
  (interactive)
  (let ((left-margin (if (save-excursion
                           (beginning-of-line)
                           (looking-at (concat timecard-entry-regex "\\|^\t\t\t")))
                         24 0)))
    (newline-and-indent)))

(defun timecard-indent-line () "Indent line with 3 tabs"
  (if (save-excursion
        (beginning-of-line)
        (delete-horizontal-space)
        (looking-at paragraph-start))
      (if (< (move-to-column left-margin) left-margin)
          (indent-to-column left-margin))
      (indent-to-left-margin)))         ; this is a text line

;;}}}
;;{{{ mstring, time-string, hhmm-string, run-command

(defmacro timecard-mstring (n)
  (` (buffer-substring (match-beginning (, n)) (match-end (, n)))))

(defmacro timecard-hhmm-string (minutes)
  (` (if (> (, minutes) 0)
         (format "%02d:%02d" (/ (, minutes) 60) (% (, minutes) 60))
       "")))

(defun timecard-time-string (&optional arg)
  (let ((now (substring (current-time-string) 11 16)))
    (cond ((and (numberp arg) (< arg 0))
	   (setq arg (+ (timecard-eval-timestr (timecard-time-string)) arg)
		 arg (if (> 0 arg) (+ arg (* 24 60)) arg))
	   (timecard-hhmm-string arg))
	  ((numberp arg) (format "%02d:%02d" (/ arg 100) (% arg 100)))
	  (t now))))
         
(defmacro timecard-run-command (cmd)
  "start an asynchronous CMD process and then ignore it..."
  (` (and (, cmd)
          (funcall 'start-process (, cmd) timecard-command-buffer
                 "/bin/sh" "-c" (, cmd)))))

;;}}}
;;{{{ date-string, eval-timestr

(defun timecard-date-string ()
  (let* ((ctime  (current-time-string))
	 (month (substring ctime 4 7))
	 (day (string-to-int (substring ctime 8 10)))
	 (separator (if timecard-numeric-date ?/ ?-)))
    (if timecard-numeric-date
	(setq month (cdr (assoc month
				'(("Jan" . "01") ("Feb" . "02") ("Mar" . "03")
				  ("Apr" . "04") ("May" . "05") ("Jun" . "06")
				  ("Jul" . "07") ("Aug" . "08") ("Sep" . "09")
				  ("Oct" . "10") ("Nov" . "11") ("Dec" . "12"))))))
    (if timecard-european-date
	(format "%02d%c%s" day separator month)
      (format "%s%c%02d" month separator day))))

(defun timecard-eval-timestr (tstr &optional relative)
  "convert time HH:MM to number of minutes since midnight.
    If optional argument RELATIVE is specified, then 24 hours are added to
    time if it is not later than the RELATIVE time specified"
    (let ((time (+ (* 60 (string-to-int (substring tstr 0 2)))
                   (string-to-int (substring tstr 3 5)))))
      (if (and relative (< time relative))
          (+ time (* 24 60))
        time)))

;;}}}
;;{{{ insert-border, narrow-to-week

(defun timecard-insert-border ()
  "Inserts a delimiter into the buffer to separates weeks"
  (interactive)
  (beginning-of-line)
  (insert "\n" (make-string 77 ?-)
	  "\n" (make-string 77 ?-) "\n\n"))

(defun timecard-narrow-to-week ()
  (let ((st (save-excursion (re-search-backward timecard-border-regex nil 'go)
                            (end-of-line) (point)))
        (en (save-excursion (re-search-forward timecard-border-regex nil 'go)
                            (or (eobp) (beginning-of-line)) (point))))
    (narrow-to-region st en)))

;;}}}
;;{{{ rate stuff

(defmacro timecard-get-mult (acct)
  (` (or (cdr (assoc (, acct) timecard-rate-alist)) 100)))

(defmacro timecard-apply-rate (acct time)
  (` (/ (* (, time) (timecard-get-mult (, acct))) 100)))

(defun timecard-read-rates ()
  "Scan the buffer for all the rate modifiers before the next border.
If the buffer is narrowwed to one week, then read the rates for that week.
Normally, this function will be called twice: first on the whole buffer to
read the default rates, next on the week being summarrized to read the rates
for that week."
  (save-excursion
    (beginning-of-buffer)
    ;; first few lines of files if they look like borders but aren't
    (while (looking-at timecard-border-regex)
      (forward-line 1))
    ;; find the first border and don't search past it.
    (re-search-forward timecard-border-regex nil 'go)

    (let (acct mult rec (limit (point)))
      (beginning-of-buffer)
      (while (re-search-forward timecard-rate-regex limit 'go)
        (setq acct (timecard-mstring 1)
              mult (string-to-int (timecard-mstring 2))
              rec (assoc acct timecard-rate-alist))
        (if rec
            (setcdr rec mult)
          (setq timecard-rate-alist (cons (cons acct mult)
                                          timecard-rate-alist)))
        ))))

;;}}}

;;{{{ checkin

;;;###autoload
(defun timecard-checkin (&optional arg)
  "Add starting time to timecard.

The optional argument can be a numeric time in 24 hour format, or an
unspecified argument which indicates that this entry is a continuation of
the last one.  No argument indicates the current time.

    ^U \\[timecard-checkin]             continue last entry
    ^U ^U \\[timecard-checkin]          continue current entry
    ^U -10 \\[timecard-checkin]		checkin 10 minutes ago
    ^U 1000 \\[timecard-checkin]        checkin at 10:00 am"

  (interactive "P")

  (timecard-find-timecard)
  (cond ((and (consp arg) (eq (car arg) 16))
	 ;;
	 ;; continue current entry (middle of file)
	 ;;
	 (let (date stime)
	   (end-of-line)
	   (re-search-backward timecard-entry-regex)
	   (setq date (timecard-mstring 1)
		 stime (if timecard-no-continuations
			   (timecard-mstring 3)
			 "    >"))
	   ;; find beginning of next entry before next empty line
	   (re-search-forward (mapconcat 'identity
					 (list timecard-entry-regex
					       timecard-checked-in-regex
					       timecard-summary-regex)
					 "\\|")
			      (save-excursion
				(re-search-forward "^[ \t]*$" nil 'go))
			      'go 2)
	   (beginning-of-line)
	   (insert date ":\t" stime " - \n")
	   (forward-char -1)))
	(t
	 ;;
	 ;; normal checkin (at end of file)
	 ;;
	 (timecard-goto-eotc t)
	 (cond ((save-excursion
		  (beginning-of-line)
		  (looking-at timecard-checked-in-regex))
		(timecard-insert-checkout (and (numberp arg) arg))
		(save-excursion (timecard-insert-checkin '(4))))
	       (t
		(timecard-insert-checkin arg t)
		(and (interactive-p)
		     (timecard-run-command timecard-checkin-command)))))))

(defun timecard-insert-checkin (arg &optional check-for-newday)
  (and check-for-newday
       (save-excursion
	 (and (re-search-backward
	       (concat "^\\(" timecard-date-regex "\\)") nil t)
	      (not (equal (buffer-substring (match-beginning 1) (match-end 1))
			  (timecard-date-string)))))
       (insert "\n\n\n----- "
	       (substring (current-time-string) 0 3)
	       " -----\n"))
  (insert ?\n (timecard-date-string) ":\t"
	  (cond ((and (consp arg)
		      (not timecard-no-continuations)) "    >")
		((consp arg)
		 (save-excursion
		   (if (re-search-backward timecard-entry-regex nil t)
		       (timecard-mstring 3) (timecard-time-string))))
		(t (timecard-time-string arg)))
          " - "))

;;}}}
;;{{{ checkout

(defun timecard-insert-checkout (arg)
  (insert (timecard-time-string arg) "\t*"))

;;;###autoload
(defun timecard-checkout (&optional arg)
  "Checkin if necessary, then add ending time to timecard.

If timecard-checkin is called, then the optional argument to
timecard-checkout is passed to timecard-checkin as well.

Timecard-checkout only understands numeric arguments, which indicate a time
in 24 hour format.  No argument indicates the current time.

    ^U -10 \\[timecard-checkout]        checkout 10 minutes ago
    ^U 1400 \\[timecard-checkout]	checkout at 2:00 pm"

  (interactive "P")

  (timecard-find-timecard)

  (cond ((and (not (save-excursion (skip-chars-forward " \t\n") (eobp)))
	      (save-excursion (beginning-of-line)
			      (looking-at timecard-checked-in-regex)))
	 (or (numberp arg)
	     (error "must specify time as argument"))
	 (insert (format "%02d:%02d" (/ arg 100) (% arg 100)) "\t*"))
	(t  (timecard-goto-eotc t)
	    (if (save-excursion
		  (beginning-of-line)
		  (looking-at timecard-checked-in-regex))
		(timecard-insert-checkout arg)
	      (timecard-insert-checkin arg))
	    (and (interactive-p)
		 (timecard-run-command timecard-checkout-command)))))

;;}}}

;;{{{ sum-day, sum-all-days

(defun timecard-sum-day ()
  "add up the hours for a block of timecard entries delimited by blank lines"
  (interactive)
  (let (day tin tout time last-time last-tout emark tallies total)

    ;; find end of day's timecard entries
    (beginning-of-line)
    (cond ((looking-at "^\\s *$")
           (skip-chars-backward " \t\n")))
    (re-search-forward "^\\s *$" nil 'go)
    (goto-char (match-beginning 0))
    (delete-horizontal-space)
    (setq emark (point-marker))
    (if (save-excursion (forward-line -1) (looking-at timecard-summary-regex))
        (kill-line -1))
      
    ;; find beginning
    (forward-char -1)
    (re-search-backward "^\\s *$" nil t)

    (save-restriction
      (narrow-to-region (point) emark)
      (while (re-search-forward timecard-entry-regex nil 'go)

        ;; under what day shall this entry be recorded?
        (or day (setq day (timecard-mstring 1)))

        ;; calculate number of minutes for this entry
        (setq tin (timecard-mstring 2)
              tout (timecard-mstring 3)
              last-time 0)
        (cond ((string-match "^ " tin)
               (or tallies
                   (error "First entry of day must be have a start time"))
               (setq tin last-tout
                     last-time (car tallies)
                     tallies (cdr tallies))))
        (setq last-tout tout
              tin (timecard-eval-timestr tin)
              tout (timecard-eval-timestr tout tin)
              time (+ last-time (- tout tin))
              tallies (cons time tallies)))

      (or (bolp) (insert ?\n))
      (setq total (apply '+ tallies))
      (insert (format "%s:: [ %s ] = %d min = %s\n"
                      day (timecard-hhmm-string total) total
                      (mapconcat 'int-to-string (nreverse tallies) " + ")))
      (forward-line -1))))


(defun timecard-sum-all-days ()
  "Redo all the daily totals for the week."
  (save-excursion
    (beginning-of-buffer)
    (while (re-search-forward
            (concat "^\\s *\n" (substring timecard-entry-regex 1)) nil t)
      (timecard-sum-day))))

;;}}}
;;{{{ sum-week

(defun timecard-sum-week ()
  "Totals up all the summary lines between between two week delimiters."
  (interactive)

  ;; clear the modifier list
  (setq timecard-rate-alist nil)

  ;; read global rates
  (timecard-read-rates)

  (let ((tally 0)
        day days drec
        acct accts arec atrec abilled
        time tin tout last-tout)
    (save-restriction
      (timecard-narrow-to-week)

      ;; read local rates
      (timecard-read-rates)

      (tabify (point-min) (point-max))
      (timecard-sum-all-days)
      (beginning-of-buffer)

      (while (re-search-forward timecard-entry-regex nil 'go)
        (goto-char (1- (match-beginning 0)))

        ;; under what day shall this entry be recorded?
        (if (or (null day) (eq (preceding-char) ?\n))
            (setq day (timecard-mstring 1)
                  drec (list day 0 nil)
                  days (cons drec days))
          (setq drec (assoc day days)))
        (forward-char 2)

        ;; find/create the account, if it's "" that's okay
        (setq acct (timecard-mstring 4))
        (or (setq atrec (assoc acct accts))
            (setq atrec (list acct 0 0)
                  accts (cons atrec accts)))
        (or (setq arec (assoc acct (nth 2 drec)))
            (progn (setq arec (cons acct 0))
                   (setcar (nthcdr 2 drec) (cons arec (nth 2 drec)))))

        ;; calculate number of minutes for this entry
        (setq tin (timecard-mstring 2)
              tout (timecard-mstring 3))
        (if (string-match "^ " tin)
            (setq tin last-tout))             
        (setq last-tout tout
              tin (timecard-eval-timestr tin)
              tout (timecard-eval-timestr tout tin)
              time (- tout tin)
              abilled (timecard-apply-rate acct time))

        ;; update the tallies
        (setq tally (+ tally abilled))  ; weekly grand total
        (setcdr arec (+ (cdr arec) time)) ; daily total for this acct
        (setcar (cdr drec) (+ (nth 1 drec) abilled)) ; daily total
        (setcar (cdr atrec) (+ (nth 1 atrec) time)) ; weekly account total
        )

      (timecard-goto-eotc)
      (setq accts (nreverse accts)
            days (nreverse days))
      (insert "\n\n\n" timecard-grandtotal-str
                 " [ " (timecard-hhmm-string tally) " ]\n\n")

      (insert timecard-date/hours-str)
      (mapcar (function
               (lambda (atrec)
                 (let ((width (+ 2 (max 5 (length (car atrec))))))
                   (setcar (nthcdr 2 atrec) width)
                   (insert (format (format "%%-%ds" width)
                                   (if (string= "" (car atrec))
                                       "other" (car atrec)))))))
              accts)
      (insert ?\n)
        
      (while days
        (setq drec (car days)
              day (car drec))
        (insert (format "%-7s%-7s " day (timecard-hhmm-string (nth 1 drec))))

        (mapcar
         (function
          (lambda (atrec)
            (let ((arec (assoc (car atrec) (nth 2 drec))))
              (insert (format (format "%%-%ds" (nth 2 atrec))
                              (if arec (timecard-hhmm-string (cdr arec)) ""))))))
         accts)
        (insert ?\n)
        (setq days (cdr days)))
                  
      (insert ?\n)
      (if (null timecard-rate-alist)
	  ;; I realize (now that I don't use them at all) how totally bogus
	  ;; my original concept of "rates" was....  *sigh*
	  ;; So, if there no "rates," then don't clutter up the output.
	  nil
	;; print totals for each account
	(insert (format "%-14s " timecard-billed-str))
	(mapcar (function
		 (lambda (atrec)
		   (insert (format (format "%%-%ds" (nth 2 atrec))
				   (timecard-hhmm-string
				    (timecard-apply-rate (car atrec)
							 (nth 1 atrec)))))))
		accts)
	(insert ?\n)

	;; print totals for each account
	(insert (format "%-14s " timecard-unbilled-str))
	(mapcar (function
		 (lambda (atrec)
		   (insert (format (format "%%-%ds" (nth 2 atrec))
				   (timecard-hhmm-string
				    (- (nth 1 atrec)
				       (timecard-apply-rate (car atrec)
							    (nth 1 atrec))))))))
		accts)
	(insert ?\n)
	)
      
      ;; print totals for each account
      (insert (format "%-14s " timecard-subtotals-str))
      (mapcar (function
               (lambda (atrec)
                 (insert (format (format "%%-%ds" (nth 2 atrec))
                                 (timecard-hhmm-string (nth 1 atrec))))))
              accts)
      (insert ?\n)

      ;; move point to beginning of the week's summary for review.
      (re-search-backward (concat "^" timecard-grandtotal-str) nil t)
      (if (featurep 'hilit19) (hilit-rehighlight-buffer))
      )))

;;}}}

;;{{{ menu definitions for FSF19 & Lucid

(and timecard-define-menus
     (cond ((string-match "Lucid" emacs-version)
            ;; Lemacs stuff
            (defvar timecard-mode-menu
              '("Timecard"
                ["Checkin" timecard-checkin t]
                ["Checkout" timecard-checkout t]
                "----"
                ["Insert border" timecard-insert-border t]
                ["Sum day" timecard-sum-day t]
                ["Sum week" timecard-sum-week t])
              "Menu for timecard mode")
            (defvar timecard-global-menu
              '("Timecard"
                ["Checkin" timecard-checkin t]
                ["Checkout" timecard-checkout t])
              "Menubar menu for timecard")
            (defun timecard-menu ()
              (interactive)
              (popup-menu timecard-mode-menu))
            (define-key timecard-mode-map 'button3 'timecard-menu)
            (add-menu nil (car timecard-global-menu) (cdr timecard-global-menu)))
           ((string-match "^19" emacs-version)
            (defvar timecard-mode-menu (make-sparse-keymap "Timecard Menu")
              "Menu for timecard mode")
	    (define-key timecard-mode-menu [sumweek] 
	      '("Sum week" . timecard-sum-week))
	    (define-key timecard-mode-menu [sumday] 
	      '("Sum day" . timecard-sum-day))
	    (define-key timecard-mode-menu [ins-border] 
	      '("Insert border" . timecard-insert-border))
	    (define-key timecard-mode-menu [nop] 
	      '("----" . undefined))
	    (define-key timecard-mode-menu [checkout] 
	      '("Check out" . timecard-checkout))
	    (define-key timecard-mode-menu [checkin] 
	      '("Check in" . timecard-checkin))

	    (define-key timecard-mode-map [menu-bar timecard]
	      (cons "Timecard" timecard-mode-menu))
            (define-key timecard-mode-map [S-down-mouse-2] timecard-mode-menu)

	    (defvar timecard-global-menu (make-sparse-keymap "Timecard")
	      "Globally enabled menu for timecard commands.")
	    (define-key timecard-global-menu [find-card]
	      '("Find timecard" . timecard-find-timecard))
	    (define-key timecard-global-menu [checkout]
	      '("Check out" . timecard-checkout))
	    (define-key timecard-global-menu [checkin] 
	      '("Check in" . timecard-checkin))

            (define-key global-map [menu-bar timecard]
              (cons "Timecard" timecard-global-menu))
            )))

;;}}}
;;{{{ timecard.sh (shell script -- AS IS)

;; --------------------------------------------------------------------------
;; #!/bin/sh
;; #
;; # timecard.sh - timecard maintainer for use with timecard-mode.el
;; #
;; #	timecard [-noedit] [-action] [text...]
;; #
;; # This does a variety of things.  The action of the script is determined
;; # by either the -action specified on the command line or the basename of the
;; # name under which the program was executed.  Stores timestamps in $WLOG.
;; # This file should have "-*- timecard -*-" on it's first line and 
;; # timecard-mode.el should be in your load-path.
;; #
;; # Copyright (c) 1993	Jonathan Stigelman (Stig@hackvan.com)
;; #
;; # This program is free software; you can redistribute it and/or modify
;; # it under the terms of the GNU General Public License as published by
;; # the Free Software Foundation; either version 2 of the License, or
;; # (at your option) any later version.
;; # 
;; # This program is distributed in the hope that it will be useful,
;; # but WITHOUT ANY WARRANTY; without even the implied warranty of
;; # MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
;; # GNU General Public License for more details.
;; # 
;; # You should have received a copy of the GNU General Public License
;; # along with this program; if not, write to the Free Software
;; # Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;;
;; # Examples:
;; #	timecard -ci		# checkin with no comment
;; #	timecard -co 		# checkout and pop up an emacs window
;; #	timecard -co "Hey, the gdb firmware stub works!"
;; #
;; # Use the force:  Read the SOURCE!
;; #
;; # Copyright (c) 1993   Jonathan Stigelman (Stig@hackvan.com)
;; #
;; #
;; 
;; case $1 in
;;   -noedit) TIMECARD_NOEDIT=x ; export TIMECARD_NOEDIT; shift ;;
;; esac
;; 
;; case $1 in
;;   -*) ACTION=`echo $1 |sed 's/^-//'`; shift ;;
;;   *)  ACTION=`basename $0` ;;
;; esac
;; 
;; [ "$WLOG" ] || WLOG=$TIMECARD
;; [ "$WLOG" ] || WLOG=$HOME/notes/log
;; WLOCK=$WLOG.lock
;; 
;; WL=$0
;; NOW="`date +%H:%M`" 
;; 
;; set_led() {
;; if [ "$DISPLAY" ]; then
;;   if [ -f $WLOCK ]; then
;;     xset led on &
;;   else
;;     xset led off &
;;   fi
;; fi
;; }
;; 
;; invoke_editor() {
;;     [ -z "$WINDOWID" -a $# = 0 ] && 
;;         XTERM="xterm -geometry 80x20-200+0 +sb -title WORK_LOG -e" 
;;     [ "$TIMECARD_NOEDIT" ] || eval $XTERM emacs -nw +9999 $WLOG
;; }
;; 
;; case $ACTION in
;; ci|in|punchin|clockin)	
;;     [ -f $WLOCK ] && { 
;; 	$WL -clockout 
;; 	NOW="    >"
;;     }
;;     touch $WLOCK
;;     set_led
;;     [ $# -ge 1 ] && echo "$* ... " > $WLOCK
;;     echo '' >> $WLOG
;;     echo -n "`date +%m/%d`:	$NOW - " >> $WLOG
;;     ;;
;; co|in|punchin|clockout)	
;;     [ -f $WLOCK ] || { 
;; 	echo ''>>$WLOG
;; 	echo -n "`date +%m/%d`:	??:?? - " >> $WLOG
;;     }
;;     echo -n "$NOW	`cat $WLOCK`$*" >> $WLOG
;;     rm $WLOCK
;;     set_led
;;     [ $# = 0 ] && invoke_editor
;;     ;;
;; ed|edit) invoke_editor ;;
;; setled)  set_led ;;
;; test) [ -f $WLOCK ] && exit 0 || exit 1 ;;
;; sofar) perl -e 'while (<>) { 
;; 		     m%^\d\d/\d\d:: \[ (\d\d):(\d\d)% || next;
;; 		     print; $hours += $1; $mins += $2; 
;; 		 } 
;; 		 $hours += ($mins/60); $mins %= 60;
;; 		 printf "\n TOTAL %02d:%02d\n\n", $hours, $mins;' $WLOG ;;
;; week) perl -e 'while (<>) { 
;; 		     m%^-----------% && do { $totals="";
;; 					     $hours= $mins=0; 
;; 					     next; };
;; 		     m%^\d\d/\d\d:: \[ (\d\d):(\d\d)% || next;
;; 		     $totals .= $_; $hours += $1; $mins += $2; 
;; 		 } 
;; 		 $hours += ($mins/60); $mins %= 60;
;; 		 print $totals;
;; 		 printf "\n TOTAL %02d:%02d\n\n", $hours, $mins;' $WLOG ;;
;; tail)
;;     ed - $WLOG <<'_eof_'
;;         $
;;         ?^------?,$p
;;         q
;; _eof_
;;     ;;
;; *)  echo "$0: unknown action ($ACTION)"; exit 1 ;;
;; esac
;; 

;;}}}

(provide 'timecard-mode)

;; Local Variables:
;; byte-compile-compatibility: t
;; End:
