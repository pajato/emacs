;;; java-setup --- Support for Java coding.

;;; Commentary:

;;; Code:

(defun java-setup ()
  "Provide enhanced Java support."
  (require 'cc-defs)
  ;;(require 'meghanada)
  ;;(require 'javadoc-lookup)
  ;;(require 'javadoc-import)
  (require 'whitespace)
  (setq tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80 84 88 92)
        indent-tabs-mode nil
        tab-width 4
        c-basic-offset 4
        fill-column 100
        whitespace-line-column 100
        c-comment-start-regexp "\\(@\\|/\\(/\\|[*][*]?\\)\\)")
  (c-set-offset 'inexpr-class 0)
  (c-set-offset 'case-label '+)
  (add-hook 'before-save-hook 'delete-trailing-whitespace)
  (add-hook 'c-mode-common-hook 'flyspell-prog-mode))

;; Use Javadoc-lookup.
(global-set-key (kbd "C-h j") 'javadoc-lookup)

;; Initialize the Jtags tags table to the default Java being used.
(setq tags-table-list (cond
                       ((eq system-type 'Xdarwin)
                        (list (format "%s/src" (getenv "JAVA_HOME"))))
                       (t nil)))

(provide 'java-setup)
;;; java-setup.el ends here
