(defun strip-mode-line ()
  "Strip the first line if it contains a mode spec."
  (interactive)
  (if (looking-at "/\\* -\\*-mode:java;")
      (kill-line 1))
  (save-buffers-kill-terminal t))


(defun test (one two three)
  "Test function"
  (message (format "The answer is %d" (+ one two three))))

(defun dired-open ()
  (interactive)
  (dired-do-async-shell-command
   "echo.sh" current-prefix-arg
   (dired-get-marked-files t current-prefix-arg)))

