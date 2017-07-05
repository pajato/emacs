;;; package --- Support for Gradle coding.

;;; Commentary:

;;; Code:
(defun gradle-setup ()
  "Some documentation..."
  (require 'compile)
  (require 'cc-vars)

  (setq indent-tabs-mode nil
        tab-width 2
        c-basic-offset 2
        fill-column 120)

  (add-hook 'before-save-hook 'delete-trailing-whitespace))

(provide 'gradle-setup)
;;; gradle-setup.el ends here
