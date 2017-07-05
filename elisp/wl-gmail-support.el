;; wanderlust
(normal-top-level-add-to-load-path
 '("/usr/share/emacs/site-lisp"
   "/usr/share/emacs/site-lisp/apel"
   "/usr/share/emacs/site-lisp/flim"
   "/usr/share/emacs/site-lisp/semi"
   "/usr/share/emacs/site-lisp/wl"))

(autoload 'wl "wl" "Wanderlust" t)
(autoload 'wl-other-frame "wl" "Wanderlust on new frame." t)
(autoload 'wl-draft "wl-draft" "Write draft with Wanderlust." t)

;; IMAP
(setq elmo-imap4-default-server "localhost")
(setq elmo-imap4-default-user "pmr")
(setq elmo-imap4-default-authenticate-type 'clear)
(setq elmo-imap4-default-port '30993)
(setq elmo-imap4-default-stream-type 'ssl)

(setq elmo-imap4-use-modified-utf7 t) 

;; SMTP
(setq wl-smtp-connection-type 'smtp)
(setq wl-smtp-posting-port 30025)
(setq wl-smtp-authenticate-type nil)
(setq wl-smtp-posting-user nil)
(setq wl-smtp-posting-server "localhost")
(setq wl-local-domain "pajato.com")

(setq wl-default-folder "%inbox")
(setq wl-default-spec "%")
(setq wl-draft-folder "%[Gmail]/Drafts") ; Gmail IMAP
(setq wl-trash-folder "%[Gmail]/Trash")

(setq wl-folder-check-async t) 

(setq elmo-imap4-use-modified-utf7 t)

(autoload 'wl-user-agent-compose "wl-draft" nil t)
(if (boundp 'mail-user-agent)
    (setq mail-user-agent 'wl-user-agent))
(if (fboundp 'define-mail-user-agent)
    (define-mail-user-agent
      'wl-user-agent
      'wl-user-agent-compose
      'wl-draft-send
      'wl-draft-kill
      'mail-send-hook))
