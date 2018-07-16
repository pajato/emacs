;; Support for the unmatched Org Mode outlining extensions...
(autoload 'org-setup "org-setup" "\
Added to have org-mode setup tailored for formatting and more...

\(fn)" t nil)

(add-hook 'org-mode-hook 'org-setup)

(defun ded/org-show-next-heading-tidily ()
  "Show next entry, keeping other entries closed."
  (interactive)
  (if (save-excursion (end-of-line) (outline-invisible-p))
      (progn (org-show-entry) (show-children))
    (outline-next-heading)
    (unless (and (bolp) (org-on-heading-p))
      (org-up-heading-safe)
      (hide-subtree)
      (error "Boundary reached"))
    (org-overview)
    (org-reveal t)
    (org-show-entry)
    (show-children)))

(defun ded/org-show-previous-heading-tidily ()
  "Show previous entry, keeping other entries closed."
  (interactive)
  (let ((pos (point)))
    (outline-previous-heading)
    (unless (and (< (point) pos) (bolp) (org-on-heading-p))
      (goto-char pos)
      (hide-subtree)
      (error "Boundary reached"))
    (org-overview)
    (org-reveal t)
    (org-show-entry)
    (show-children)))

(defun indent-entries ()
  "Iterate over the file indenting each entry following + at the beginning of a line."
  (interactive)
  (let ((pos (point))
        (done (point)))
    (while done
      (fill-paragraph)
      (setq done (re-search-forward "^+ " nil t)))
    (goto-char pos)))

(provide 'org-support)
