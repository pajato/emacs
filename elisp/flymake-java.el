;;; -*-EMACS-LISP-*-
;;; Flymake for Java using Eclipse ECJ
;;;
;;; Adapted from 
;;; http://www.credmp.org/index.php/2007/07/20/on-the-fly-syntax-checking-java-in-emacs/
;;; and removed JDE requiresments.
;;;
;;; To use:
;;; (defun my-java-mode-hook ()
;;;   (define-key (current-local-map) "\C-c\C-f" 'flymake-mode))
;;; (add-hook 'java-mode-hook 'my-java-mode-hook)
;;; (require 'flymake-java)
;;; (add-hook 'flymake-mode-hook 'my-flymake-mode-hook)
;;; (defun my-flymake-mode-hook () 
;;;  (define-key (current-local-map) "\C-c\C-d" 'flymake-display-err-menu-for-current-line)
;;;  (define-key (current-local-map) "\C-c\C-n" 'flymake-goto-next-error)
;;;  (define-key (current-local-map) "\C-c\C-D" 'credmp/flymake-display-err-minibuf))
;;;

(require 'flymake)
 
(defconst ecj-jar-path "c:/path/to/ecj-3.4M4.jar")

(defvar flymake-java-version "1.5")

;;; If use you ant, try this target to determine the classpath
;;;  <target name="show_classpath" >
;;;    <pathconvert targetos="windows" property="classpath.string" refid="the-classpath-ref-from-javac-entry" setonempty="true" />
;;;    <echo message="classpath=${classpath.string}"/>
;;;  </target>
;;; This will convert the classpath to be in Windows OS format.  
;;; Doing it on Linux is no challenge, so it's not documented here.
(defvar flymake-java-classpath "c:/path/to/project/classes;c:/path/to/lib/foo.jar")

(defun flymake-java-ecj-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                     'flymake-ecj-create-temp-file))
         (local-file (file-relative-name
                      temp-file
                      (file-name-directory buffer-file-name))))
    (list "java" (list "-jar" ecj-jar-path "-Xemacs" "-d" "none" 
                       ;; "-warn:none"
                       "-source" flymake-java-version "-target" flymake-java-version "-proceedOnError"
                       "-classpath" flymake-java-classpath
                       ;; "-log" "c:/temp/foo.xml"
                       local-file))))

(defun flymake-java-ecj-cleanup ()
  "Cleanup after `flymake-java-ecj-init' -- delete temp file and dirs."
  (flymake-safe-delete-file flymake-temp-source-file-name)
  (when flymake-temp-source-file-name
    (flymake-safe-delete-directory (file-name-directory flymake-temp-source-file-name))))
 
(defun flymake-ecj-create-temp-file (file-name prefix)
  "Create the file FILE-NAME in a unique directory in the temp directory."
  (file-truename (expand-file-name (file-name-nondirectory file-name)
                                   (expand-file-name  (int-to-string (abs (random))) (flymake-get-temp-dir)))))
    
(push '(".+\\.java$" flymake-java-ecj-init flymake-java-ecj-cleanup) flymake-allowed-file-name-masks)
 
(defun credmp/flymake-display-err-minibuf () 
  "Displays the error/warning for the current line in the minibuffer"
  (interactive)
  (let* ((line-no             (flymake-current-line-no))
         (line-err-info-list  (nth 0 (flymake-find-err-info flymake-err-info line-no)))
         (count               (length line-err-info-list)))
    (while (> count 0)
      (when line-err-info-list
        (let* ((file       (flymake-ler-file (nth (1- count) line-err-info-list)))
               (full-file  (flymake-ler-full-file (nth (1- count) line-err-info-list)))
               (text (flymake-ler-text (nth (1- count) line-err-info-list)))
               (line       (flymake-ler-line (nth (1- count) line-err-info-list))))
          (message "[%s] %s" line text)))
      (setq count (1- count)))))

(provide 'flymake-java)