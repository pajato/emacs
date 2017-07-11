;;; package --- Personal Prelude setup.
;;;
;;; Commentary:
;;;
;;; Setup autoloads and hooks to customize the many, many Emacs libraries used.
;;;
;;; Code:

;; Use Google Drive for personal Emacs lisp libraries.
(add-to-list 'load-path (expand-file-name "~/gd/emacs/elisp"))

;; Provide Prelude preferred support for Windows and Mac dependent features.
(require 'terminal-support)
(require 'emacs-support)
(require 'environment-support)
(require 'prelude-helm-everywhere)
;(require 'go-support)
(require 'gradle-support)
(require 'java-support)
(require 'keyboard-support)
;;(require 'kotlin-support)
(require 'maven-support)
(require 'navigation-support)
(require 'net-utils-support)
(require 'outline-support)
(require 'org-support)
(require 'shell-support)
(require 'sgml-support)

(load-theme 'wheatgrass)

;;; Cursor color choices: ("#92c48f" "#6785c5" "#be369c" "#d9ca65")
(add-to-list 'default-frame-alist '(cursor-color . "#be369c"))
(blink-cursor-mode)

;;(set-frame-parameter nil 'fullscreen (when (not (frame-parameter nil 'fullscreen)) 'fullboth))
(dired "~/Projects")

(setq magit-last-seen-setup-instructions "1.4.0")

;; Setup modifier keys for the emacs mac port from Mituharu-san so
;; that Cocoa behavior prevails.
(setq mac-option-modifier 'meta
      mac-command-modifier 'super)

(provide 'init)
;;; init.el ends here
