;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cond
 ((equal host "preilly")
  (setq mail-default-reply-to "preilly@ll.mit.edu"
        mail-yank-prefix " > "
        mail-yank-ignored-headers "^via:\\|^mail-from:\\|^origin:\\|^status:\\|^remailed\\|^received:\\|^message-id:\\|^summary-line:\\|^to:\\|^subject:\\|^in-reply-to:\\|^return-path:\\|^date:\\|^from:\\|^references:\\|^content-type:\\|^content-length:\\|^cc:"
        mail-archive-file-name "~/Mail/Sent-mitll"
        mail-send-hook nil
        mail-self-blind nil
        smtpmail-default-smtp-server "localhost"
        smtpmail-local-domain nil
        send-mail-function 'smtpmail-send-it))
 ((equal host "roamer")
  (setq mail-default-reply-to "pmr@pajato.com"
        user-mail-address "pmr@pajato.com"
        mail-yank-prefix " > "
        mail-yank-ignored-headers "^via:\\|^mail-from:\\|^origin:\\|^status:\\|^remailed\\|^received:\\|^message-id:\\|^summary-line:\\|^to:\\|^subject:\\|^in-reply-to:\\|^return-path:\\|^date:\\|^from:\\|^references:\\|^content-type:\\|^content-length:\\|^cc:"
        mail-archive-file-name "~/Mail/Sent-roamer"
        mail-send-hook nil
        mail-self-blind nil
        smtpmail-default-smtp-server "localhost"
        smtpmail-local-domain nil
        send-mail-function 'smtpmail-send-it))
 ((equal host "daun")
  (setq mail-default-reply-to "pmr@pajato.com"
        user-mail-address "pmr@pajato.com"
        mail-yank-prefix " > "
        mail-yank-ignored-headers "^via:\\|^mail-from:\\|^origin:\\|^status:\\|^remailed\\|^received:\\|^message-id:\\|^summary-line:\\|^to:\\|^subject:\\|^in-reply-to:\\|^return-path:\\|^date:\\|^from:\\|^references:\\|^content-type:\\|^content-length:\\|^cc:"
        mail-archive-file-name "~/Mail/Sent"
        mail-send-hook nil
        mail-self-blind nil
        mail-program-name "aspell"
        mail-send-hook 'ispell-message
        smtpmail-smtp-service 465
        smtpmail-default-smtp-server "smtp.googlemail.com"
	smtpmail-starttls-credentials '(("smtp.googlemail.com" 465 nil nil))
        send-mail-function 'smtpmail-send-it))
 ((equal host "copa")
  (setq mail-default-reply-to "pmr@pajato.com"
      user-mail-address "pmr@pajato.com"
      mail-yank-prefix " > "
      mail-yank-ignored-headers "^via:\\|^mail-from:\\|^origin:\\|^status:\\|^remailed\\|^received:\\|^message-id:\\|^summary-line:\\|^to:\\|^subject:\\|^in-reply-to:\\|^return-path:\\|^date:\\|^from:\\|^references:\\|^content-type:\\|^content-length:\\|^cc:"
      mail-archive-file-name "~/Mail/Sent"
      mail-program-name "aspell"
      mail-send-hook 'ispell-message
      mail-self-blind nil
      rmail-preserve-inbox t))
 (t
  (setq mail-default-reply-to "pmr@pajato.com"
      user-mail-address "pmr@pajato.com"
      mail-yank-prefix " > "
      mail-yank-ignored-headers "^via:\\|^mail-from:\\|^origin:\\|^status:\\|^remailed\\|^received:\\|^message-id:\\|^summary-line:\\|^to:\\|^subject:\\|^in-reply-to:\\|^return-path:\\|^date:\\|^from:\\|^references:\\|^content-type:\\|^content-length:\\|^cc:"
      mail-archive-file-name "~/mail/Sent"
      mail-program-name "aspell"
      mail-send-hook 'ispell-message
      mail-self-blind nil
      rmail-preserve-inbox t)))

(make-face 'mail-to-face)
(set-face-foreground 'mail-to-face "green")
(set-face-background 'mail-to-face "black")

(make-face 'mail-cc-face)
(set-face-foreground 'mail-cc-face "green")
(set-face-background 'mail-cc-face "black")

(make-face 'mail-bcc-face)
(set-face-foreground 'mail-bcc-face "green")
(set-face-background 'mail-bcc-face "black")

(make-face 'mail-from-face)
(set-face-foreground 'mail-from-face "red")
(set-face-background 'mail-from-face "black")

(make-face 'mail-cited-face)
(set-face-foreground 'mail-cited-face "maroon")
(set-face-background 'mail-cited-face "black")

(make-face 'mail-cited-author-face)

(make-face 'mail-separator-face)
(set-face-foreground 'mail-separator-face "wheat")
(set-face-background 'mail-separator-face "black")

(make-face 'mail-copy-face)
(set-face-foreground 'mail-copy-face "red")
(set-face-background 'mail-copy-face "black")

(make-face 'mail-subject-face)
(set-face-foreground 'mail-subject-face "green")
(set-face-background 'mail-subject-face "black")

(make-face 'mail-other-face)

(make-face 'mail-fcc-face)
(set-face-foreground 'mail-fcc-face "red")
(set-face-background 'mail-fcc-face "black") 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Define mail keywords

(setq mail-font-lock-keywords
  '(;; Fontify To field
    ("^\\(To: .*\\)$" 1 'mail-to-face)
    ;; Fontify Carbon Copy field
    ("^\\([Cc][Cc]: .*\\)$" 1 'mail-cc-face)
    ;; Fontify In-reply-to field
    ("^\\(In-reply-to: .*\\)$" 1 'mail-cited-face)
    ;; Fontify In-reply-to field
    ("^\\(Reply-to: .*\\)$" 1 'mail-cited-face)
    ;; Fontify Subject
    ("^\\(Subject: .*\\)$" 1 'mail-subject-face)
    ;; Fontify File Carbon Copy field.
    ("^\\(FCC: .*\\)$" 1 'mail-fcc-face)
    ;; Fontify Blind Carbon Copy field.
    ("^\\(BCC: .*\\)$" 1 'mail-bcc-face)
    ;; Fontify seperator.
    ("^\\(--text follows this line--\\)$" 1 'mail-separator-face)
    ;; Fontify Start of forward.
    ("^\\(------- Start of forwarded message -------\\)$" 1 'mail-separator-face)
    ;; Fontify End of forward.
    ("^\\(------- End of forwarded message -------\\)$" 1 'mail-separator-face)
    ;; Fontify Cited Reply author field
    ("^\\(.* writes:\\)$" 1 'mail-cited-author-face)
    ;; Fontify return path
    ("^\\(Return-Path: .*\\)$" 1 'mail-other-face)
    ;; Fontify date 
    ("^\\(Date: .*\\)$" 1 'mail-other-face)
    ;; Fontify From .
    ("^\\(From: .*\\)$" 1 'mail-from-face)
    ;; Fontify Mime version 
    ("^\\(M[Ii][Mm][Ee]-Version: .*\\)$" 1 'mail-other-face)
    ;; Fontify X-Mailer 
    ("^\\(X-Mailer: .*\\)$" 1 'mail-other-face)
    ;; Fontify X-Sender 
    ("^\\(X-Sender: .*\\)$" 1 'mail-other-face)
    ;; Fontify Content-Type 
    ("^\\(Content-Type: .*\\)$" 1 'mail-other-face)
    ;; Fontify Content-Length 
    ("^\\(Content-Length: .*\\)$" 1 'mail-other-face)
    ;; Fontify cited lines
    ("^\\([ ]*>.*\\)$" 1 'mail-cited-face)))
;; Additional expressions to highlight in mail mode.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; IMAP Mail support using ViewMail (VM)

(cond
 ((or (string= host "wurb") (string= host "daun"))
  (setq vmdir "~/lib/emacs/elisp/elpa/vm/")
  (add-to-list 'load-path (expand-file-name (concat vmdir "lisp/")))
  (add-to-list 'Info-default-directory-list (expand-file-name (concat vmdir "info/")))
  (require 'vm-autoloads)

  ;; Use VM by default
  (setq mail-user-agent 'vm-user-agent))
 (t nil))

(provide 'mail-support)
