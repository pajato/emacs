(dired "~/workspace/Manage")
(make-shell "$tomcat")
(make-shell "$su")
(make-shell "$manage")
(make-shell "$db")
(let ((proc (get-buffer-process (current-buffer))))
  (insert "mysql --user=test --password=test --database=manage_db")
  (comint-send-input))
