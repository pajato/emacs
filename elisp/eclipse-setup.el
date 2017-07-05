;; Support for leveraging Eclipse tools and operations when working
;; with Java, Go, C, C++, Ruby, Python, PHP, ...
(defun eclipse-setup ()
  "Dummy function used to autoload this file.")

;;; Support for miscellaneous Eclipse functions, especially via the
;;; emacs-eclim facility.

(defun get-eclipse-home-dir ()
  (cond
   ((eq system-type 'darwin) "/Users/pmr/Downloads/eclipse")
   ((eq system-type 'windows-nt) "C:\Program Files\Eclipse Foundation\eclipse")
   ((eq system-type 'linux) "/tbd")
   (t nil)))

(let ((eclipse-home-dir (get-eclipse-home-dir)))
  (setq eclimd-executable (concat eclipse-home-dir "/plugins/org.eclim_2.2.5/bin/eclimd")
        eclimd-default-workspace "/Users/pmr/Documents/workspace"
        eclim-executable (concat eclipse-home-dir "/plugins/org.eclim_2.2.5/bin/eclim"))
  (add-to-list 'load-path (expand-file-name "~/gd/emacs/elisp/emacs-eclim"))
  (autoload 'eclim-setup "eclipse-setup" "\
Setup to leverage integration with the Eclipse IDE.
Permit the use of Eclipse operations via it's command line API
in order to provide enhanced IDE functionality within Emacs.

\(fn)" t nil)
  (when (file-accessible-directory-p eclipse-home-dir)

    ;; Support for leveraging Eclipse tools and operations when working
    ;; with Java, Go, C, C++, Ruby, Python, PHP, ...
    (require 'eclim)
    (require 'eclimd)
    (require 'auto-complete-config)
    (require 'ac-emacs-eclim-source)
    (require 'company)
    (require 'company-emacs-eclim)
    (require 'eclim-project)
    (require 'eclim-maven)

    ;; Configure auto completion and general eclim mode.
    (global-eclim-mode)
    (ac-config-default)
    (ac-emacs-eclim-config)
    (company-emacs-eclim-setup)
    (global-company-mode t)))

