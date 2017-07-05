;;; esh --- Emacs SHell mode.  A substitute for the standard shell mode.
;;;
;;; M-x esh name will go to the buffer named $name if it exists,
;;; otherwise it willl be created.

(defun esh (name)
  "Switch to buffer '$NAME' if it exists, otherwise create it."
  (interactive "sShell Buffer: ")
  (let ((buffer (get-buffer (concat "$" name)))
        (shell-name (concat "$" name)))
    (if (null buffer)
        (shell (get-buffer-create shell-name)))
    (switch-to-buffer shell-name)))
