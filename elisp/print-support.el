(cond
 ((and (eq system-type 'windows-nt) (equal (get-domain) "topic-radio.com"))
  (setq ps-printer-name "\\\\comrouter\\lexmark"))
 ((equal (get-domain) "dhcp.gte.com")
  (setq ps-printer-name "lj5simx"))
 (t (setq ps-printer-name "prtone")))

(provide 'print-support)
