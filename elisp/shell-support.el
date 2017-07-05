(require 'shell)

;; Add hooks to prevent the "kill on exit" query and to strip ^M characters.
(add-hook 'comint-exec-hook 
	  (lambda () (process-kill-without-query (get-buffer-process (current-buffer)))))
(add-hook 'comint-output-filter-functions 'comint-strip-ctrl-m)

;; Peform the Emacs shell initialization.
(add-hook 'eshell-mode-hook 'eshell-setup)

(defun eshell-setup ()
  (local-set-key "\C-u" 'eshell-kill-input))

;; Create a named Emacs shell buffer
(defun esh (name)
  "Switch to buffer '$NAME' if it exists, otherwise create it."
  (interactive "sEshell Buffer: ")
  (let ((eshell-buffer-name (concat "$" name)))
    (eshell)))

;; Ripped off (with much thanks!) from http://www.khngai.com/emacs/eshell.php
(defun eshell/clear ()
  "04Dec2001 - sailor, to clear the eshell buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)))

(defun execute-shell-commands (buffer command-list)
  "Execute the commands given in COMMAND-LIST using the shell in BUFFER."
  (let ((buf-proc (get-buffer-process buffer)))
    (while command-list
      (comint-send-string buf-proc (car command-list))
      (comint-send-string buf-proc "\n")
      (setq command-list (cdr command-list)))))

(defun execute-eshell-commands (buffer command-list)
  "Execute the commands given in COMMAND-LIST using the shell in BUFFER."
  (let ((buf-proc (get-buffer-process buffer)))
    (while command-list
      (comint-send-string buf-proc (car command-list))
      (comint-send-string buf-proc "\n")
      (setq command-list (cdr command-list)))))

(defun make-shell (name &optional hostname username password)
  "Create a shell buffer named NAME on HOSTNAME for USERNAME using
PASSWORD to login."
  (interactive "sName: ")
  (setq name (concat "$" name))
  (cond
   ((get-buffer name)
    (message "%s buffer already exists" name))
   ;; If all three optional arguments are provided the user wants us
   ;; to register the data with the ange-ftp package.  With just two
   ;; of the optional arguments the user wants us to "get" the password
   ;; in a secure fashion.  With just a hostname the user wants us to use
   ;; defaults.  In each case we will save the info for future ange-ftp
   ;; sessions.
   ((and hostname username password)
    (ange-ftp-set-passwd hostname username password)
    (shell)
    (rename-buffer name)
    (goto-char (point-max))
    (insert "rlogin " hostname " -l " username)
      ;;; Handle password, if required using `M-x send-invisible'
    (comint-send-input))
   ((and hostname username)
    (ange-ftp-get-passwd hostname username)
    (shell)
    (rename-buffer name)
    (goto-char (point-max))
    (insert "rlogin " hostname " -l " username)
    ;; Handle password, if required using `M-x send-invisible'
    (comint-send-input))
   (hostname
    (let ((username (user-login-name)))
      (ange-ftp-set-passwd hostname username
			   (ange-ftp-get-passwd hostname username))
      (shell)
      (rename-buffer name)
      (goto-char (point-max))
      (insert "rlogin " hostname " -l " username)
      ;; Handle password, if required using `M-x send-invisible'
      (comint-send-input)))
   ;; 
   (t
    (shell)
    (rename-buffer name))))

(defun run-in-shell (name command)
  "Execute COMMAND in the shell NAME"
  (interactive)
  (make-shell name)
  (execute-shell-commands (concat "$" name) (list command)))

(defun sh (name)
  "Switch to buffer '$NAME' if it exists, otherwise create it."
  (interactive "sShell Buffer: ")
  (let ((buffer (get-buffer (concat "$" name)))
        (shell-name (concat "$" name)))
    (if (null buffer)
        (make-shell name))
    (switch-to-buffer shell-name)))

(defun chrome-instance ()
  (interactive)
  (run-in-shell "chrome" "google-chrome.sh"))

(defun dev-instance ()
  (interactive)
  (run-in-shell "dev" "emacs --debug-init --title=DEV --maximized"))

(defun main-instance ()
  (interactive)
  (run-in-shell "main" "emacs --debug-init --title=MAIN --fullheight --geometry=160-150"))

(defun su-instance ()
  (interactive)
  (run-in-shell "su" "sudo emacs --title=SU --fullheight --geometry=160-150"))

(defun shell-init ()
  "Initialize the shell configurations for this system."
  (interactive)
  (let ((host (get-host-name))
        (display (getenv "DISPLAY"))
        (domain (get-domain)))
    ;; Run these on all hosts
    (frame-set-name "SERVER")
    (main-instance)
    (su-instance)
    (dev-instance)
    (chrome-instance)

    ;; Set up host based shell configurations
    (cond
     ((and (string= host "daun")
           (or (string= display "daun")
               (string= display ":0.0")
	       (string= display ":0"))))
     ((string= host "reipa09")
;;      (run-in-shell "socks" "sshx -D 8898 pmr@hamm.pajato.com")
;;      (run-in-shell "firefox" "firefox")
      (run-in-shell "mount" "mount.sh")
      (run-in-shell "synergy" "synergys -f -n reipa09"))
     ((string= host "conf")
      (run-in-shell "synergy" "synergyc -f -n parl 192.168.2.4"))
     ((string= host "vent")
      (run-in-shell "synergy" "synergys -f -n vent")))))

(provide 'shell-support)
