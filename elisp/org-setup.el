;;; package --- Setup library for *.org files.
;;;
;;; Commentary:
;;;
;;; Provide some additional functions for Org.

;;; Code:
(require 'org)
(require 'plantuml-mode)

;; A couple of functions to help naviate Org-mode presentations.

(defun ded/org-show-next-heading-tidily ()
  "Show next entry, keeping other entries closed."
  (interactive)
  (if (save-excursion (end-of-line) (outline-invisible-p))
      (progn (org-show-entry) (outline-show-children))
    (outline-next-heading)
    (unless (and (bolp) (org-at-heading-p))
      (org-up-heading-safe)
      (outline-hide-subtree)
      (error "Boundary reached"))
    (org-overview)
    (org-reveal t)
    (org-show-entry)
    (outline-show-children)))

(defun ded/org-show-previous-heading-tidily ()
  "Show previous entry, keeping other entries closed."
  (interactive)
  (let ((pos (point)))
    (outline-previous-heading)
    (unless (and (< (point) pos) (bolp) (org-at-heading-p))
      (goto-char pos)
      (outline-hide-subtree)
      (error "Boundary reached"))
    (org-overview)
    (org-reveal t)
    (org-show-entry)
    (outline-show-children)))

;; Support for morphing a transcription to an outline.

(defun pti/indent-entries ()
  "Iterate over the file indenting each entry following + at the beginning of a line."
  (interactive)
  (let ((pos (point))
        (done (point)))
    (while done
      (fill-paragraph)
      (setq done (re-search-forward "^+ " nil t)))
    (goto-char pos)))

;; Establish the defaults for an Org buffer.
(defun org-setup ()
  "Establish some buffer local attributes."
  (setq fill-column 120
        whitespace-line-column 120))

;; Establish one-time attributes.

(setq org-directory "~/org"
      org-default-notes-file (concat org-directory "/notes.org")
      org-ditaa-jar-path "~/td/Pajato/members/pmr/lib/ditaa0_8.jar"
      org-plantuml-jar-path "~/td/Pajato/members/pmr/lib/plantuml.jar")

(org-babel-do-load-languages
 'org-babel-load-languages
 '(;; other Babel languages
   (plantuml . t)))
;;; org-setup.el ends here
