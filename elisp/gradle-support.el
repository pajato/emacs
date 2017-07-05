;;; package --- Support for Groovy and Gradle

;;; Commentary:

;;; Code:

(autoload 'groovy-mode "groovy-mode" "Major mode for editing Groovy code." t)
(autoload 'run-groovy "inf-groovy" "Run an inferior Groovy process")
(autoload 'inf-groovy-keys "inf-groovy" "Set local key defs for inf-groovy in groovy-mode")
(add-to-list 'auto-mode-alist '("\.groovy$" . groovy-mode))
(add-to-list 'auto-mode-alist '("\.gradle$" . groovy-mode))
(add-to-list 'interpreter-mode-alist '("groovy" . groovy-mode))

;; Support 'next-error handling from Emacs' compile mode.
;;(add-to-list 'compilation-error-regexp-alist '("^\[ERROR\] \(.*\):\[\([0-9]+\),\([0-9]+\)\]" 1 2 3))

;;; make Groovy mode electric by default.
;;(add-hook 'groovy-mode-hook '(lambda ()
;;             (require 'groovy-electric)
;;             (groovy-electric-mode)))

;;(add-hook 'groovy-mode-hook 'gradle-setup)

(provide 'gradle-support)
;;; gradle-support.el ends here
