(defun mm-setup ()
  (setq tab-stop-list
        '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80)
        indent-tabs-mode nil
        tab-width 4
        c-basic-offset 4))

(add-hook 'java-mode-hook 'mm-setup)
(add-hook 'actionscript-mode-hook 'mm-setup)
(add-hook 'nxml-mode-hook 'mm-setup)

(defmacro c-identifier-re (re)
  `(concat "\\<\\(" ,re "\\)\\>[^_]"))

;; Keywords defining protection levels
(defconst c-protection-key "\\<\\(public\\|protected\\|private\\)\\>")


;;(setq load-path (cons (substitute-in-file-name "/usr/local/share/emacs/site-lisp/cc-mode-5.28") load-path))
;;(require 'cc-mode)

(require 'actionscript-mode)
(add-to-list 'auto-mode-alist '("\\.as[123]?$" . actionscript-mode))
(add-to-list 'auto-mode-alist '("\\.mxml$" . nxml-mode))

(provide 'mm-support)
