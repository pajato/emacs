;;; NOTE: This file is likely stale, post prelude.
;;;
;;; Main FSF Emacs startup file.
;;;

(require 'cl)
(defvar *emacs-load-start* (current-time))

;; Setup autoloads and hooks to customize the many, many Emacs libraries used.
(add-to-list 'load-path (expand-file-name "~/emacs/elisp"))
(require 'library-support)

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

(let ((time (current-time)))
  (message "Emacs startup: %ds, %dus"
           (- (second time) (second *emacs-load-start*))
           (- (third time) (third *emacs-load-start*))))

(put 'dired-find-alternate-file 'disabled nil)
