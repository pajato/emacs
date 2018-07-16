;;; kotlin-support --- Support for Kotlin

;;; Commentary:

;;; Code:

;; Support for the Kotlin programming language
(require 'fill-column-indicator)
(autoload 'kotlin-setup "kotlin-setup" "\
Added to have kotlin-mode setup tailored behavior for formatting and more...

\(fn)" t nil)
(add-hook 'kotlin-mode-hook 'kotlin-setup)
(add-hook 'kotlin-mode-hook 'fci-mode)
(setq fci-rule-width 1
      fci-rule-color "darkblue")

(provide 'kotlin-support)
;;; kotlin-support ends here
