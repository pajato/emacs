;;;
;;; Emacs Library Setup
;;;
;;; dummy change to unwedge google drive!

;;; The following are order independent and provided in all Emacsi 
;;; running in the current session.

;;; As part of good Emacs practice, each require below evaluates a
;;; feature support file that sets up autoloads or implements simple,
;;; fast operations.
(require 'backup-support)
;;(require 'completion-support)
(require 'eclipse-support)
(require 'elpa-support)
(require 'emacs-support)
(require 'environment-support)
(require 'frame-support)
(require 'go-support)
(require 'gradle-support)
(require 'java-support)
(require 'keyboard-support)
(require 'maven-support)
(require 'navigation-support)
(require 'net-utils-support)
(require 'outline-support)
(require 'org-support)
(require 'shell-support)
(require 'sgml-support)

;;; Stale library setup:
;(require 'C-support)
;(require 'TeX-support)	; TeX word processing support.
;(require 'alarm-support)
;(require 'ange-ftp-support)
;(require 'bookmark-support)
;(require 'buffer-support)
;(require 'cc-mode-support)
;(require 'crypt-support)
;(require 'dictionary-support)
;(require 'dired-support)
;(require 'dos-support)
;(require 'efs-support)
;(require 'environment-support)
;(require 'faq-support)
;(require 'font-support)
;(require 'font-lock-support)
;(require 'jsp-support)
;(require 'lispdir-support)
;(require 'mail-support)
;(require 'manpage-support)
;(require 'misc-functions)
;(require 'mm-support)
;(require 'muse-support)
;(require 'mode-line-support)
;(require 'psg-support)
;(require 'paths-support)
;(require 'print-support)
;(require 'privacy-support)
;(require 'project-support)
;(require 'rmail-support)
;(require 'rpm-support)
;(require 'scrolling-support)
;(require 'spell-support)
;(require 'tar-support)
;(require 'terminal-support)
;(require 'texinfo-support)
;(require 'text-mode-support)
;(require 'timecard-support)
;(require 'vc-support)
;(require 'w3m-support)
;(require 'what-line-support)
;(load-library "which")
;(load-library "misc")

(provide 'library-support)
