;;; package --- Personal Prelude setup.
;;;
;;; Commentary:
;;;
;;; Setup autoloads and hooks to customize the many, many Emacs libraries used.
;;;
;;; Code:

;; Use Google Drive for personal Emacs lisp libraries.
(add-to-list 'load-path (expand-file-name "~/td/Pajato/members/pmr/emacs/elisp"))

;; Provide Prelude preferred support for Windows and Mac dependent features.
(require 'general-support)
(require 'keybinding-support)
;(require 'environment-support)
;(require 'prelude-helm-everywhere)
;(require 'go-support)
;(require 'git-support)
;(require 'gradle-support)
;(require 'java-support)
;(require 'keyboard-support)
;(require 'kotlin-support)
;(require 'linum-support)
;(require 'maven-support)
;(require 'navigation-support)
;(require 'net-utils-support)
;(require 'outline-support)
(require 'org-support)
;(require 'shell-support)
;(require 'sgml-support)
;(require 'epresent)

(load-theme 'wheatgrass)

;;; Cursor color choices: ("#92c48f" "#6785c5" "#be369c" "#d9ca65")
(add-to-list 'default-frame-alist '(cursor-color . "#be369c"))
(blink-cursor-mode)

;;; Start up in the Projects folder.
(dired "~/Projects")

;;; Not sure why this is here.
(setq magit-last-seen-setup-instructions "1.4.0")

(provide 'init)
;;; init.el ends here
