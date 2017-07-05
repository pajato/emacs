;; Buffer Management

;;(autoload 'buffer-menu-dired-extended "GTbuf-men")
;;(define-key ctl-x-map "\C-b" 'buffer-menu-dired-extended)

(load-library "new-buf-sel")
(global-set-key "\C-xb" 'select-buffer)
(global-set-key "\C-x4b" 'select-buffer-other-window)
(global-set-key [C-prior] 'beginning-of-buffer)
(global-set-key [C-next] 'end-of-buffer)

(setq buffer-select-output-kill-message nil)

(defun bs-create-buffer (buf)
  "Create a new buffer and switch to it."
  (interactive "sCreate buffer: ")
  (switch-to-buffer (generate-new-buffer buf)))

(defun bs-show-buffer-filename ()
  "Print the name of the file associated with the current buffer."
  (interactive)
  (message (buffer-file-name)))

(define-key ctl-x-map "B" 'bs-create-buffer)

(define-key help-map "\C-b" 'bs-show-buffer-filename)

(provide 'buffer-support)
