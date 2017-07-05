(require 'outline)

(defun outline-start-entry ()
  (interactive)
  (insert (concat "  Entry start: " (current-time-string) "\n\n  ")))

(defun outline-stop-entry ()
  (interactive)
  (insert (concat "\n\n  Entry stop: " (current-time-string) "\n")))

(define-key outline-mode-prefix-map "A" 'outline-start-entry)
(define-key outline-mode-prefix-map "Z" 'outline-end-entry)

;; Insure that outline mode will be applied, but only once.
(let ((found nil)
      (modes auto-mode-alist))
  (while (and (not found) modes)
    (if (string= "\\.otl\\'" (car (car modes)))
	(setq found t))
    (setq modes (cdr modes)))
  (if (not found)
      (setq auto-mode-alist (append '(("\\.otl\\'" . org-mode)) auto-mode-alist)))
  auto-mode-alist)

(provide 'outline-support)

