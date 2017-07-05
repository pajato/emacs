(require 'info)

(cond
 ((string= host "copa")
  (setq Info-directory-list
        (append Info-directory-list
                (list "/usr/share/info"
                      "/usr/global/info"
                      "/usr/local/info"))))
 ((string= host "lilly")
  (setq Info-directory-list
        (append Info-directory-list
                (list "/usr/share/info"
                      "/usr/global/info"
                      "/usr/local/info"))))
 ((string= host "akers")
  (setq Info-directory-list (list "C:/Emacs/emacs-21.2/info")))
 ;; Ignore other systems.
 ((string= host "localhost")
  (setq Info-directory-list
        (append Info-directory-list
                (list "/home/pmr/projects/gnu/info"
                      "/usr/local/info"
                      "/usr/share/info"))))
 (t nil))

(setq kill-emacs-hook
      (function (lambda ()
                  (and (featurep 'info-bookmark)
                      Info-bookmark-alist
                       (y-or-n-p "Save info-bookmarks? ")
                       (Info-bookmark-write nil)))))

;;; Set up an INFO frame if we are running in a windowed emacs.
;;(frame-setup-new-frame "info" "82x48+110+0" 'info)

(defun Info-find-emacs-lisp-nodes (item)
  "Return a list of locations documenting ITEM in the Emacs Lisp manual.
The locations are of the format used in Info-history, i.e.
\(FILENAME NODENAME BUFFERPOS\)."
  (require 'info)
  (let ((where '())
	(cmd-desc (concat "^\\* " (regexp-quote (symbol-name item))
			  ":\\s *\\(.*\\)\\.$")))
    (save-excursion
      (Info-find-node "elisp" "Index")
      ;; Take the index node off the Info history.
      (setq Info-history (cdr Info-history))
      (goto-char (point-max))
      (while (re-search-backward cmd-desc nil t)
	  (setq where (cons (list Info-current-file
				  (buffer-substring
				   (match-beginning 1)
				   (match-end 1))
				  0)
			    where)))
      where)))

(defun Info-goto-emacs-lisp-node (item)
  "Go to the Info node in the Emacs Lisp manual for item ITEM, which can be a
function, macro or variable.
The command is found by looking up in the Emacs Lisp manual's Index."
  (interactive "SFind documentation for item: ")
  (let ((where (Info-find-emacs-lisp-nodes item)))
    (if where
	(let ((num-matches (length where)))
	  ;; Get Info running, and pop to it in another window.
	  (save-window-excursion
	    (info))
	  (pop-to-buffer "*info*")
	  (Info-find-node (car (car where))
			  (car (cdr (car where))))
	  (if (> num-matches 1)
	      (progn
		;; Info-find-node already pushed (car where) onto
		;; Info-history.  Put the other nodes that were found on
		;; the history.
		(setq Info-history (nconc (cdr where) Info-history))
		(message (substitute-command-keys
			  "Found %d other entr%.  Use \\[Info-last] to see %s."
			(1- num-matches)
			(if (> num-matches 2) "ies" "y")
			(if (> num-matches 2) "them" "it"))))))
      (error "Couldn't find documentation for %s." item))))

(define-key global-map "\b'" 'Info-goto-emacs-lisp-node)

(provide 'info-support)
