;;; java-support --- Support for Java

;;; Commentary:

;;; Code:

;; Support for the Java programming language
(require 'fill-column-indicator)
(add-to-list 'load-path (expand-file-name "~/gd/emacs/elisp/javadoc-lookup.proj"))
(autoload 'java-setup "java-setup" "\
Added to have java-mode setup tailored behavior for formatting and more...

\(fn)" t nil)
(add-hook 'java-mode-hook 'java-setup)
(add-hook 'java-mode-hook 'fci-mode)
(setq fci-rule-width 1
      fci-rule-color "darkblue")

;; Set up meghanada mode
;;(require 'meghanada-mode-setup)

(provide 'java-support)
;;; java-support ends here
