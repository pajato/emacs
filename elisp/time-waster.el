;; Build a time waster buffer for today in /tmp/date
(let ((name "~/tmp/tw.org")
      (start (string-to-number (nth 0 (split-string (nth 3 (split-string (current-time-string) " ")) ":")))))
  (find-file name)
  (erase-buffer)
  (insert (format "Time wastage for %s\n\n" (current-time-string)))
  (insert "| Hour | Wasteage |\n")
  (insert "|---\n")
  (insert "| <5> | <60> |\n")
  (dotimes (count (- 24 start))
    (insert (format "|%d:00||\n" (+ count start))))
  (insert "|---\n"))

