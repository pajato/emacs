;; Support for the unmatched Org Mode outlining extensions...
(autoload 'org-setup "org-setup" "\
Added to have org-mode setup tailored for formatting and more...

\(fn)" t nil)

(add-hook 'org-mode-hook 'org-setup)

(provide 'org-support)
