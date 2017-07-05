;;; completion-support.el --- Support for abbreviation expansion

;; Use the new and improved auto-complete library.

(add-to-list 'load-path "~/gd/emacs/elisp/auto-complete/")
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/gd/emacs/elisp/ac-dict")
(ac-config-default)

;; Stale:
;; Provide support for handling abbreviations using two mechanisms
;; recommended by Trey Jackson: Predictive Abbreviation (pabbrev) and
;; dynamic text expansion (hippie-expand).

;; pabbrev is an external Emacs lisp library.  hippie-expand is part
;; of Emacs.

;(require 'pabbrev)

;(define-key global-map [M-/] 'hippie-expand)


(provide 'completion-support)
