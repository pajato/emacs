;;; environment-support.el --- Environment setup for the host, domain and display.

;;; Commentary:

;;; Provides functions to set up the current Emacs environment.

;;; Code:

(defun find-sentinel-path (filename)
  "Return nil or the path to file FILENAME by traversing from the current directory to the root."
  (let* ((dir (expand-file-name default-directory))
         (parent (file-name-directory (directory-file-name dir)))
         (path (concat dir filename)))
    (while (and (not (file-readable-p path))
                (not (string= parent dir)))
      (setq dir parent
            parent (file-name-directory (directory-file-name dir))
            path (concat dir filename)))
    (if (not (string= dir parent))
        path)))

(defun get-domain ()
  "Return the domain name of the system or nil if not connected to the network."
  ;; Case on the operating system.
  (cond
   ;; Handle a windows system by extracting the domain name from the
   ;; IPCONFIG command.
   ((or (eq system-type 'windows-nt) (eq system-type 'cygwin))
    (let ((output (shell-command-to-string "ipconfig /all"))
          domain)
      (setq domain
            (and
             (or (string-match
                  "Primary DNS Suffix[ .]*: \\([a-zA-Z0-9.]+\\)
" output)
                 (string-match
                  "Connection-specific DNS Suffix[ .]*: \\([a-zA-Z0-9.]+\\)
" output)
                 (string-match
                  "DNS Suffix Search List[ .]*: \\([a-zA-Z0-9.]+\\)
" output))
             (substring output (match-beginning 1) (match-end 1))))
      ;; Detect the empty string to indicate no network.
      (if (and domain (= 0 (length domain)))
          nil
        domain)))
   ((eq system-type 'gnu/linux)
    (let ((output (shell-command-to-string "domainname -d"))
          domain)
      (setq domain (substring output 0 (1- (length output))))
      domain))
   ((eq system-type 'darwin)
    (let ((fullname (shell-command-to-string "hostname"))
          (shortname (shell-command-to-string "hostname -s"))
          domain)
      (setq fullname (substring fullname 0 (1- (length fullname)))
            shortname (substring shortname 0 (1- (length shortname)))
            domain (substring fullname (1+ (length shortname))))))
   (t (error "Unhandled system type: %s" system-type))))

(defun get-host-name ()
  "Get the host machine name, sans domain info."
  (let ((h (system-name)))
    (cond
     ((null h)
      ;; Report on an unamed system.
      (message "This system does not have a hostname!")
      "localhost")
     ((string-match "[a-zA-Z0-9]+" h)
      (substring h 0 (match-end 0)))
     (t (message "This system does not have a valid hostname!")
        "localhost"))))

(defun get-display-name ()
  "Get the machine name, sans domain info."
  (let* ((d (getenv "DISPLAY"))
         (L (length d))
         (h (get-host-name))
         (i 0))
    (cond
     ;; Check for a dumb terminal; return null.
     ((null window-system) "")
     ;; Check for no X running.
     ((or (null d) (equal 'w32 window-system)) h)
     ;; Check for a local display; return the local host name.
     ((and (= 0 (or (string-match ":[.0-9]+" d) -1))
           (= 0 (string-match "[a-zA-Z0-9]+" h)))
      (substring h 0 (match-end 0)))

     ;; Look for a Mac OS X display string
     ((and
       (eq system-type 'darwin)
       (or (string= d h)
           (and (not (null (string-match "/tmp.+:0" d)))
                (= 0 (string-match "/tmp.+:0" d)))))
      h)

     ;; Check for a remote display of the form `name:#[.#]'; return the
     ;; whole display string.
     ((= 0 (or (string-match "\\([a-zA-Z0-9]+\\)[.a-zA-Z0-9]*:[.0-9]+" d) -1))
      d)

     ;; Check for the Emacs Mac Port
     ((string= d "Mac")
      h)

     ;; Check for a weird display.
     ((and d (> (length d) 5) (string= (substring d -5) "::0.0"))
      (substring d 0 -5))

     ;; Check for Windows/NT on an intel platform.
     ((string= "i386-*-nt" (getenv "PLATFORM"))
      h)

     ;; Default to return null.
     (t ""))))

(defconst env:platform (getenv "PLATFORM"))
(defconst env:host (downcase (get-host-name)))
(defconst env:display (get-display-name)
  "The display name for the running Emacs session.")
(defconst env:frame-platform (downcase (format "%s-%s" env:host env:display))
  "Identifies the host and the display this instance of Emacs is utilizing.")

;; Identifies the instance of Emacs as identified by the `-name
;; INSTANCE' command line arguments.
(defconst env:instance (if window-system
                       (cdr (assoc 'name (frame-parameters (selected-frame))))
                     nil))

;;; Provide support for the environment on Mac OS X

(defun generate-env-plist ()
  "Dump the current environment into the ~/.MacOSX/environment.plist file and conditionally mount the video drive."
  ;; The system environment is found in the global variable:
  ;; 'initial-environment' as a list of "KEY=VALUE" pairs.
  (let ((list initial-environment)
        pair start command key value dirname)
    ;; Make sure the directory exists.
    (setq dirname "~/.MacOSX")
    (or (file-exists-p dirname) (make-directory dirname))

    ;; clear out the current environment settings
    (find-file "~/.MacOSX/environment.plist")
    (goto-char (point-min))
    (if (search-forward "<dict>\n" (point-max) t)
        (progn
          (setq start (point))
          (search-forward "</dict>")
          (beginning-of-line)
          (delete-region start (point)))
      (delete-region (point-min) (point-max))
      (insert "<dict>\n</dict>\n")
      (goto-char 8))
    (while list
      (setq pair (split-string (car list) "=")
            list (cdr list))
      (setq key (nth 0 pair)
            value (nth 1 pair))
      (insert "  <key>" key "</key>\n")
      (insert "  <string>" value "</string>\n")

      ;; Enable this variable in launchd
      (setq command (format "launchctl setenv %s \"%s\"" key value))
      (shell-command command))
    ;; Save the buffer.
    (save-buffer)

    ;; Mount the data drive unless it is already mounted or if we are not at JCL.
    (shell-command "~/bin/macosx/data.sh")))

(if (or (eq window-system 'ns) (eq window-system 'darwin))
    (progn
      (add-hook 'before-save-hook
                '(lambda ()
                   (or (file-exists-p (file-name-directory buffer-file-name))
                       (make-directory (file-name-directory buffer-file-name) t))))
      (generate-env-plist)))

(provide 'environment-support)
;;; environment-support.el ends here
