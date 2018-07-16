;;; package --- Setup support for *.org files.
;;;
;;; Commentary:
;;;
;;; Use autoload to defer loading the org mode support library until
;;; org-mode is established on a file.

;;; Code:

(autoload 'org-setup "org-setup" "Load org-mode configuration." t nil)
(add-hook 'org-mode-hook 'org-setup)

(provide 'org-support)
;;; org-support.el ends here
