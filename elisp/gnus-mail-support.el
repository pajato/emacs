(require 'cl)

(setq gnus-secondary-select-methods
      '((nnimap "psg"
		(nnimap-address "localhost")
		(nnimap-server-port 30993)
		(nnimap-list-pattern ("Inbox" "Drafts" "Sent" "Junk" "Trash")))
	(nnimap "gmail"
		(nnimap-address "localhost")
		(nnimap-server-port 40993)
		(nnimap-list-pattern ("Inbox" "Trash"))
		(nnimap-stream ssl))))
	
