(require 'rmail)
(require 'rmailsum)
;;(require 'rmime)
(require 'mail-support)
(require 'browse-url)
(require 'etach)

;;;(setq rmail-font-lock-keywords mail-font-lock-keywords)

;; MIME Stuff
;(add-hook 'rmail-show-message-hook 'rmime-format)
;(add-hook 'rmail-edit-mode-hook    'rmime-cancel)
;(autoload 'rmime-format "rmime" "" nil)

;; URL button support
;;(add-hook 'rmail-show-message-hook 'rmail-arm-urls)
;;;(add-hook 'rmail-mode-hook 
;;;	  (lambda ()
;;;	    (define-key rmail-mode-map [mouse-2] 'browse-url-at-mouse)))

;; Set up firefox behavior.
(setq browse-url-new-window-flag t
      browse-url-firefox-new-window-is-tab t)

;; Customize my use of Rmail:
(setq rmail-summary-window-size (/ (- (frame-height) 2) 4)
      rmail-delete-after-output t
      rmail-confirm-expunge nil
      rmail-output-file-alist (list
			       (cons "^from: pmr" "~/Mail/Archives/family/pmr"))
      rmail-default-rmail-file "~/Mail/")

(define-key rmail-mode-map "R" 'rmail-cited-reply)
(define-key rmail-summary-mode-map "R" 'rmail-summary-cited-reply)

(defun rmail-cited-reply (just-sender)
  "Reply to a message using the default VM style citation."
  (interactive "P")
  (let ((citation (format "%s writes:\n\n" (mail-fetch-field "from"))))
    (rmail-reply just-sender)
    (insert citation)
    (mail-yank-original nil)))

(defun rmail-summary-cited-reply (just-sender)
  "Reply to the current message including the cited body in the reply.
Normally include CC: to all other recipients of original message;
prefix argument means ignore them."
  (interactive "P")
  (set-buffer rmail-buffer)
  (rmail-cited-reply just-sender))


;; Setup Rmail to handle browsing operations.
(global-set-key "\C-c\C-z." 'browse-url-at-point)
(global-set-key "\C-c\C-zb" 'browse-url-of-buffer)
(global-set-key "\C-c\C-zr" 'browse-url-of-region)
(global-set-key "\C-c\C-zu" 'browse-url)
(global-set-key "\C-c\C-zv" 'browse-url-of-file)
(add-hook 'dired-mode-hook
	  (lambda ()
             (local-set-key "\C-c\C-zf" 'browse-url-of-dired-file)))

;; Browse URLs in mail messages under RMAIL by clicking mouse-2:
(add-hook 'rmail-mode-hook (lambda () ; rmail-mode startup
  (define-key rmail-mode-map [mouse-2] 'browse-url-at-mouse)))
;; Alternatively, add `goto-address' to `rmail-show-message-hook'.

(provide 'rmail-support)

