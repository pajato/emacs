;; VM support

(require 'mail-support)

(require 'u-vm-color)
(add-hook 'vm-summary-mode-hook 'u-vm-color-summary-mode)
(add-hook 'vm-select-message-hook 'u-vm-color-fontify-buffer)

(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "google-chrome"
      mail-user-agent 'vm-user-agent
      psg-inbox "~/tmp/psg"
      gmail-inbox "~/tmp/gmail"
      psg-crash-box "~/tmp/psg.crash"
      gmail-crash-box "~/tmp/gmail.crash"
      psg-maildrop "imap-ssl:imap.googlemail.com:993:inbox:login:pmr@pajato.com:*"
      gmail-maildrop "imap-ssl:imap.gmail.com:993:inbox:login:pajatopmr:*"
      vm-url-browser 'browse-url
      vm-move-after-deleting t
      vm-move-after-killing t
      vm-expunge-before-save t
      vm-expunge-before-quit t
      vm-primary-inbox psg-maildrop
      vm-auto-get-new-mail 60
      vm-imap-expunge-after-retrieving nil
      vm-imap-account-alist `((,psg-maildrop "psg") (,gmail-maildrop "gmail"))
      vm-imap-save-to-server t
      vm-delete-after-saving t
      vm-spool-files `((,psg-inbox ,psg-maildrop ,psg-crash-box)
		       (,gmail-inbox ,gmail-maildrop ,gmail-crash-box)))
