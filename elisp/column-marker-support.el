;;; Support for adding a column marker 

(require 'column-marker)

(add-hook 'java-mode-hook (lambda () (interactive) (column-marker-1 80)))
