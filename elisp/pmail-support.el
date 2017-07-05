(require 'pmail)
(require 'pmailsum)

(setq pmail-preserve-inbox t
      pmail-display-summary nil
      pmail-movemail-program nil
      pmail-primary-inbox-list (list "imap://pmr:cpla%1995@copa.pajato.com"))

(define-key pmail-mode-map "R" 'rmail-cited-reply)
(define-key pmail-summary-mode-map "R" 'rmail-summary-cited-reply)

(defun rmail-cited-reply (just-sender)
  "Reply to a message using the default VM style citation."
  (interactive "P")
  (let ((citation (format "%s writes:\n\n" (mail-fetch-field "from"))))
    (pmail-reply just-sender)
    (with-current-buffer "*mail*"
      (insert citation)
      (mail-yank-region nil))))

(defun rmail-summary-cited-reply (just-sender)
  "Reply to the current message including the cited body in the reply.
Normally include CC: to all other recipients of original message;
prefix argument means ignore them."
  (interactive "P")
  (set-buffer rmail-buffer)
  (rmail-cited-reply just-sender))
