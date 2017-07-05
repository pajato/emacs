(require 'package-support)

(package-register "Hyperbole"
                  (concat site-lisp "/hyperbole-3.11/")
                  (concat site-lisp "/hyperbole-3.11")
                  "Slick personal management utility. Version 3.11")

;(load-library "hyperbole")
;(load-library "hsite")
;;; Hyperbole rolodex main entry points.
(autoload 'rolo-fgrep        "wrolo"      "Hierarchical rolodex"         t)
(autoload 'rolo-grep         "wrolo"      "Hierarchical rolodex"         t)

(global-set-key "\C-x4r" 'rolo-menu)

(setq smart-scroll-proportional t)
(setq auto-mode-alist (append '(("\\.otl$" . outline-mode)) auto-mode-alist))

;; Rolodex Support
(defun locate-phone-number (keyword &optional max-matches)
  "Find a phone number using the hyperbole rolo-grep function."
  (interactive "sName keyword(s): \nP")
  (find-item keyword (list "~/lib/fone-osf.otl" "~/lib/fone.otl") t))

(defun locate-folder (folder &optional max-matches)
  "Find a file folder in one the 721 Bennington or IBM filing cabinets."
  (interactive "sFolder keyword(s): \nP")
  (find-item folder (list "~/lib/folders.otl") t))

(defun locate-cd (CD &optional max-matches)
  "Find a CD located in one of the 721 Bennington CD racks."
  (interactive "sCD keyword(s): \nP")
  (let ((count (find-item CD (list "~/lib/cd-reviews.otl") t)))
    (if (> count 0)
	(save-excursion
	  (set-buffer "*Rolodex*")
	  (toggle-read-only)
	  (outline-mode)
	  (hide-body)
	  (toggle-read-only)))
    count))

(defun locate-ticket (ticket &optional max-matches)
  "Find an OSF ticket log entry."
  (interactive "sTicket: \nP")
  (find-item ticket (list "~/lib/tickets.otl") t)) 

(defun locate-cd-review (CD &optional max-matches)
  "Find a CD review."
  (interactive "sCD keyword(s): \nP")
  (find-item CD (list "~/lib/cd-reviews.otl") t))

(defun thesaurus-lookup (word &optional max-matches)
  "Find a thesaurus item."
  (interactive "sWord to match: \nP")
  (find-item word (list "/usr/gnu/lib/thesaurus.otl") t))

(defun locate-bookcase-item (keyword &optional max-matches)
  "Find an article/note/document/book stored in a bookcase."
  (interactive "sBookcase keyword(s): \nP")
  (find-item keyword (list "~/lib/bookcases.otl") t))

(defun locate-quote (keyword &optional max-matches)
  "Find a quote in the quotes database."
  (interactive "sQuote keyword(s): \nP")
  (find-item keyword (list "~/lib/quotes.otl") t))

(defun locate-channel (keyword &optional max-matches)
  "Find a channel in the channels list."
  (interactive "sChannel keyword(s): \nP")
  (find-item keyword (list "~/lib/boston-radio-channels.otl") t))

(defun locate-ticket (keyword &optional max-matches)
  "Find a ticket in the ticket comment data base."
  (interactive "sTicket keyword(s): \nP")
  (find-item keyword (list "~/lib/tickets.otl") t))
  

(defun find-item (keyword file-list &optional display-flag)
  (let ((total-matches
	 (rolo-grep (regexp-quote keyword) max-matches
		    (setq rolo-file-list file-list))))
    (if display-flag
	(message (get-match-count-message total-matches)))
    total-matches))

(defun get-match-count-message (matches)
  "Return a message indicating how many matches were found."
  (concat
   (if (= total-matches 0) "No" (format "%d" total-matches))
   " matching entr"
   (if (= total-matches 1) "y" "ies")
   " found in rolodex."))

;;; Make sure hyperbole will load the phone interface.

(add-hook 'hyperb:init-hook
	  (function (lambda ()
		      (load-library "hphone"))))

(provide 'hyperbole-support)
