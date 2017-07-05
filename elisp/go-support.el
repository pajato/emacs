;;; package --- go-support provides the hooks to invoke standard formatting.

;;; Commentary:
;;;for the Go programming language
(autoload 'go-format "go-setup" "\
Added to have go-mode setup tailored behavior for formatting.

\(fn)" t nil)

;; Ensure that golang files will be formatted prior to being saved.
(add-hook 'go-mode-hook 'go-format)

;; Include the oracle.
(load-file (concat (getenv "GOPATH") "/src/golang.org/x/tools/cmd/oracle/oracle.el"))

(provide 'go-support)
