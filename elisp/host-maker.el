;; This buffer is for notes you don't want to save, and for Lisp evaluation.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer.

(let ((buf (get-buffer-create "host"))
      (count 0)
      (host-re "^\\s-+host \\([^ ]+\\) {")
      (host-list '())
      address comment name)
  (set-buffer buf)
  (erase-buffer)
  (insert-file-contents "~/dhcpd.conf")
  (message (format "%d" (point)))
  (while (re-search-forward host-re nil t)
    (setq count (1+ count)
	  name (match-string 1))
    (forward-line -1)
    (if (looking-at "## \\(.*\\)")
	(setq comment (match-string 1))
      (error "Invalid format for name %s: missing comment." name))
    (forward-line 3)
    (if (looking-at "^\\s-+fixed-address \\([^;]+\\);")
	(setq address (match-string 1))
      (error "Invalid format for name %s: missing address." name))
    (setq host-list (append host-list (list (list comment name address)))))

  ;; Dump the hosts and addresses to a file.
  (erase-buffer)
  (while host-list
    (setq name (nth 1 (car host-list))
	  address (nth 2 (car host-list))
	  comment (nth 0 (car host-list)))
    (insert address "\t" name "\t# " comment "\n")
    (setq host-list (cdr host-list)))
  (message (format "Processed %s hosts" count)))
