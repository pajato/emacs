;;; Support for miscellaneous Eclipse functions, especially via the
;;; emacs-eclim facility.

;; Disable until company.el is integrated into my workflow.
;(add-to-list 'load-path (expand-file-name "~/gd/emacs/elisp/emacs-eclim"))
;(autoload 'eclim-setup "eclipse-setup" "\
;Setup to leverage integration with the Eclipse IDE.
;Permit the use of Eclipse operations via it's command line API
;in order to provide enhanced IDE functionality within Emacs.;
;
;\(fn)" t nil)

(provide 'eclipse-support)
