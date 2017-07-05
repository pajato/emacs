;;; Support for Emacs' VC (Version Control) mode.
;;;
;;; Map C-x v U to vc-update
(define-key 'vc-prefix-map "U" 'vc-update)

(provide 'vc-support)
