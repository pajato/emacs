;;; Support for processing the braserio XML save format to extract
;;; playlist information

(defun brasero-make-playlist ()
  "Generate a playlist from the XML spec in the current buffer"
  (interactive)
  (let ((index 1)
	(output (get-buffer-create "playlist.txt")))
    (with-current-buffer output (erase-buffer))
    (setq name-re "\\([-a-zA-Z0-9%.]+\\)"
	  song-re (format "<title>%s</title>[ \t\n]+<artist>%s</artist>"
			  name-re name-re))
    (while (re-search-forward song-re (point-max) t)
      (let ((artist (match-string 2))
	    (title (match-string 1)))
	(with-current-buffer output
	  (insert (format "%d. %s {%s}\n" index title artist))))
      (setq index (1+ index)))
    ;; Fix up the quoted strings in the output buffer
    (with-current-buffer output
      (progn
	(unquote "%20" " ")
	(unquote "%27" "'")
	(unquote "%26" "&")
	(unquote "%28" "(")
	(unquote "%29" ")")
	(unquote "%2C" ",")
	(unquote "%2F" "/")
	(unquote "%3A" ":")
	(save-buffer)))))

(defun unquote (quoted unquoted)
  (goto-char (point-min))
  (while (re-search-forward quoted nil t) (replace-match unquoted)))
