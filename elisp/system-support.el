;; Provide support for Windows and Mac dependent features.
(cond
 ((eq system-type 'darwin)
  ;; Mac OS X does not support ls --dired
  (setq ls-lisp-use-insert-directory-program nil)
  (require 'ls-lisp)

  ;; Bind the keys labeled "command" to Super.
  (setq mac-command-modifier 'super)

  ;; Put the per-user Emacs specific data under Google Drive.
  (let ((dir (expand-file-name "~/gd/emacs/.emacs.d/")))
    (setq user-emacs-directory dir
          package-user-dir dir)))
 ((eq system-type 'windows-nt)
  (require 'windows-support))
 (t nil))

(put 'dired-find-alternate-file 'disabled nil)
(provide 'system-support)
