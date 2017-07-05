;;; Support package for Jonathan Stigelman's very slick `timecard.el'
;;; package.

(load-library "timecard-mode")

(defvar ts-current-job "MITLL"

  "Keyword used to select job specific information.")

(global-set-key "\C-x\C-t"     (make-sparse-keymap)) ; if you also use v18
(global-set-key "\C-x\C-t\C-i" 'timecard-checkin)
(global-set-key "\C-x\C-t\C-o" 'timecard-checkout)
(global-set-key "\C-x\C-t\C-t" 'timecard-find-timecard)
(global-set-key "\C-c\C-t"     'timecard-continue)

(setq timecard-define-menus t)    ; only if you want menus

(autoload 'timecard-find-timecard "timecard-mode" nil t)
(autoload 'timecard-checkin       "timecard-mode" nil t)
(autoload 'timecard-checkout      "timecard-mode" nil t)
(autoload 'timecard-mode          "timecard-mode" nil t)

;; Tailor the colors more to my liking.
(if window-system 
    (setq timecard-color-alist
	  '((timecard-entry green)
	    (timecard-rate gold-bold-italic)
	    (timecard-leader Plum1-bold-italic)
	    (timecard-summary cyan-bold)
	    (timecard-date/time cyan-bold-italic)
	    (timecard-grandtotal red-bold-italic)
	    (timecard-billed green-bold-italic)
	    (timecard-date/hours/subtotals gold-bold-italic))))
  
;;; Interface to the Pencom time sheet utilities.

(setq timecard-file (concat "~/lib/" (downcase ts-current-job) "/tc.log"))

(cond
 ((string= ts-current-job "MITLL")
  (setq ts-consultant-name "Paul Reilly"
	ts-client-company "MIT Lincoln Labs."
	ts-client-manager "Sue Andrews"
	ts-client-location "Cambridge, MA"
	ts-file-ext "-mitll"
	timecard-summary-hook 'ts-make-timesheet))
 ((string= ts-current-job "VZN")
  (setq ts-consultant-name "Paul Reilly"
	ts-client-company "Verizon Labs, Inc."
	ts-client-manager "Steve Morrison"
	ts-client-location "Waltham, MA"
	ts-file-ext "-vzn"
	timecard-summary-hook 'ts-make-timesheet))
 ((string= ts-current-job "KAF")
  (setq ts-consultant-name "Paul Reilly"
	ts-client-company "Keane/Allmerica Financial"
	ts-client-manager "Bill Sutliff"
	ts-client-location "Worcester, MA"
	ts-file-ext "-kaf"
	timecard-summary-hook 'ts-make-timesheet))
 ((string= ts-current-job "KVI")
  (setq ts-consultant-name "Paul Reilly"
	ts-client-company "Knowledge Vector, Inc."
	ts-client-manager "Carl Ziegler"
	ts-client-location "Durham, NC"
	ts-file-ext "-kvi"
	timecard-summary-hook 'ts-make-timesheet))
 ((string= ts-current-job "IBM/LOTUS")
  (setq ts-consultant-name "Paul Reilly"
	ts-client-company "IBM/Lotus"
	ts-client-manager "Martha Stammers"
	ts-client-location "Westford, MA"
	ts-file-ext "-ibm"
	timecard-summary-hook 'ts-make-timesheet))
 ((string= ts-current-job "SOCK")
  (require 'eliassen-timesheets)
  (setq ts-consultant-name "Paul Reilly"
        ts-client-company "Sockeye Networks, Inc."
        ts-client-manager "Phil DiBello"
        ts-client-location "Newton, MA"
        ts-file-ext "-sock"
        timecard-summary-hook 'ts-make-timesheet))
 ((string= ts-current-job "ALLR")
  (require 'eliassen-timesheets)
  (setq ts-consultant-name "Paul Reilly"
	ts-client-company "Allaire, Inc."
	ts-client-manager "Randy Nielsen"
	ts-client-location "Newton, MA"
	ts-file-ext "-allr"
	timecard-summary-hook 'ts-make-timesheet))
 ((string= ts-current-job "EGEN")
  (require 'eliassen-timesheets)
  (setq ts-consultant-name "Paul Reilly"
	ts-client-company "Egenera, Inc."
	ts-client-manager "Pete Manca"
	ts-client-location "Bolton, MA"
	ts-file-ext "-egen"
	timecard-summary-hook 'ts-make-timesheet))
 ((string= ts-current-job "SAO")
  (require 'eliassen-timesheets)
  (setq ts-consultant-name "Paul Reilly"
	ts-client-company "Smithsonian Astrophyscis Observatory"
	ts-client-manager "Heather Volatile"
	ts-client-location "Hampshire Street, Cambridge, MA"
	ts-file-ext "-sao"
	timecard-summary-hook 'ts-make-timesheet))
 ((string= ts-current-job "ADI")
  (require 'eliassen-timesheets)
  (setq ts-consultant-name "Paul Reilly"
	ts-client-company "Analog Devices, Inc."
	ts-client-manager "Russell Meschetkin"
	ts-client-location "Rte 1, Norwood MA"
	ts-file-ext "-adi"
	timecard-summary-hook 'ts-make-timesheet))
 ((string= ts-current-job "BBN")
  (require 'eliassen-timesheets)
  (setq ts-consultant-name "Paul Reilly"
	ts-client-company "Bolt Beranek and Newman"
	ts-client-manager "Anthony Amicangioli"
	ts-client-location "733 Concord Avenue, Cambridge MA 02142"
	ts-file-ext "-bbn"
	timecard-summary-hook 'ts-make-timesheet))
 ((string= ts-current-job "OSF")
  (require 'pencom-timesheets)
  (setq ts-consultant-name "Paul Reilly"
	ts-client-company "Open Software Foundation"
	ts-client-manager "Chauncy Liu"
	ts-client-location "11 Cambridge Center, Cambridge MA 02142"
	ts-file-ext "-osf"
	timecard-summary-hook 'ts-make-ps-timesheet))
 ((string= ts-current-job "IBM")
  (require 'pencom-timesheets)
  (setq ts-consultant-name "Paul Reilly"
	ts-client-company "IBM"
	ts-client-manager "Glen Chalemin"
	ts-client-location "One Technology Center,
\t\t\t\t\t\tWestford MA 01886"
	ts-file-ext "-ibm"
	timecard-summary-hook 'ts-make-ascii-timesheet))
 ((string= ts-current-job "PSG")
  (setq timecard-summary-hook nil)))

(defun timecard-continue ()
  "Just continue the current entry.  My standard interface to timecard-checkin."
  (interactive)
  (timecard-checkin '(4)))

(provide 'timecard-support)
