
;;; Provide a function to goto a particular column on a particular
;;; line.  Takes a string spec of the form "#[:#]' where the first #
;;; is the line number and the second # is the column number.

(defun my-goto-line (position)
  "Goto position at line:column."
  (interactive "sPosition: ")
  (let ((re "\\([0-9]*\\):?\\([0-9]*\\)")
        line column)
    (if (string-match re position)
        (setq line (match-string 1 position)
              column (match-string 2 position))
      (error "Bad line!"))
    (goto-line (string-to-number line))
    (move-to-column (string-to-number column))))
