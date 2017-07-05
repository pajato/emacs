;;; java-support --- Support for Java

;;; Commentary:

;;; Code:

;; Support for the Java programming language using Meghanada
(require 'meghanada)
(add-hook 'java-mode-hook
          (lambda ()
            ;; meghanada-mode on
            (meghanada-mode t)
            (setq c-basic-offset 8)
            ;; use code format
            (add-hook 'before-save-hook 'meghanada-code-beautify-before-save)))
(provide 'meghanada-support)
;;; java-support ends here
