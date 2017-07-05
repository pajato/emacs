(let ((path load-path)
      (maildir "/export/projects/gnu/emacs/lisp/mail")
      found)
  (while path
    (if (string= (car path) maildir)
	(setq found t
	      path nil)
      (setq path (cdr path))))
  (unless found
    (setq load-path (append (list "/export/projects/gnu/emacs/lisp/mail") load-path)))
  (load "imail"))

(provide 'imail-support)








